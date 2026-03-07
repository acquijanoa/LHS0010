## Load libraries
library(sf)
library(haven)
library(dplyr)
library(spdep)
library(cmdstanr)
library(posterior)
library(tidyr)
library(ggplot2)
library(tidybayes)
library(Matrix)
library(ggpattern) 
cmdstan_version()

## read shapefile
honduras19 <- st_read("../data/shp/Shp_mgd.shp") %>% 
                  filter(CNTRYNAMEE =="Honduras") %>% 
                  mutate(DHSREGEN = ifelse(DHSREGEN == "Resto Francisco Morazan","FRANCISCO MORAZAN",DHSREGEN)) %>%
                  mutate(DHSREGEN = ifelse(DHSREGEN == "Resto Cortes","CORTES",DHSREGEN))%>% 
                  mutate(DHSREGEN=factor(toupper(DHSREGEN))) %>%
                  rename(REGION_NAME=DHSREGEN)
unique(honduras19$REGION_NAME)

## 
dept_labels <- c("ATLANTIDA", "COLON", "COMAYAGUA", "COPAN", 
                 "CORTES", "CHOLUTECA", "EL PARAISO", 
                 "FRANCISCO MORAZAN", "GRACIAS A DIOS", "INTIBUCA", 
                 "ISLAS DE LA BAHIA", "LA PAZ", "LEMPIRA", 
                 "OCOTEPEQUE", "OLANCHO", "SANTA BARBARA", 
                 "VALLE", "YORO", "SAN PEDRO SULA", "DISTRITO CENTRAL")

## load log odds estimated on sas
log.odds<- read_sas('../data/derived/hestimates_19.sas7bdat') %>% 
            select(REGION,AGEGROUP_C4,Estimate,StdErr) %>% 
            mutate(REGION_NAME = factor(REGION, levels = 1:20, labels = dept_labels)) %>% 
            filter(AGEGROUP_C4==1)
    
## We merge the shapefile
merged_shp <- left_join(honduras19, log.odds, by = 'REGION_NAME') %>%
                arrange(REGION)

R <- nrow(merged_shp)
# make the polygons valid
merged_shp <- st_make_valid(merged_shp)

# Base adjacency (touching)
nb <- poly2nb(merged_shp, queen = TRUE)

# Identify islands (no neighbors)
islands <- which(lengths(nb) == 1)

# Find containing region for each island using spatial predicate
# (the container polygon "contains" the enclave polygon)
for (i in islands) {
  container <- which(st_contains(merged_shp, merged_shp[i, ], sparse = FALSE)[,1])
  container <- setdiff(container, i)
  
  # if multiple containers, pick the first (usually there should be exactly 1)
  if (length(container) >= 1) {
    j <- container[1]
    nb[[i]] <- unique(c(nb[[i]], j))
    nb[[j]] <- unique(c(nb[[j]], i))
  }
}
# Check again
which(lengths(nb) == 1)

# Convert nb -> edge list
edges <- do.call(
  rbind,
  lapply(seq_len(R), function(i) {
    if (length(nb[[i]]) == 0) return(NULL)
    cbind(node1 = i, node2 = nb[[i]])
  })
) |> as.data.frame()
edges <- edges %>% filter(node1 < node2)
E <- nrow(edges)

# --- Stan vectors
y <- merged_shp$Estimate
v <- (merged_shp$StdErr)^2

## 
W <- nb2mat(nb, style = "B", zero.policy = TRUE) # Binary adjacency
D <- diag(rowSums(W))
Q <- D - W 

# 2. Function to calculate the scaling factor
# This follows the approach by Riebler et al. (2016)
get_scaling_factor_robust <- function(Q) {
  n <- nrow(Q)
  # Adding a slightly larger ridge to handle disconnected islands
  # then using the diagonal of the covariance matrix
  tryCatch({
    # We use a precision-to-covariance transformation
    # ensuring we handle the sum-to-zero constraint per component
    inv_Q <- solve(Q + diag(1e-6, n)) 
    scaling_factor <- exp(mean(log(diag(inv_Q))))
    return(scaling_factor)
  }, error = function(e) {
    # Fallback: If still singular, the graph is likely very fragmented
    return(1.0) 
  })
}
scaling_factor <- get_scaling_factor_robust(Q)


# define stan_data
stan_data <- list(
  R = R,
  y = y,
  v = v,
  E = E,
  scaling_factor = scaling_factor,
  node1 = edges$node1,
  node2 = edges$node2,
  estimate_kappa = 1
)

## call the model
mod <- cmdstan_model("icar_gaussian_direct.stan")

## fit the bayesian model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 10000,
  refresh = 200
)

fit$summary(c("alpha", "kappa"))
u_draws <- fit$draws("u", format = "df")
u_summary <- u_draws %>%
  summarise_draws() %>%
  # This gives you mean, median, sd, rhat, etc., for each u[1], u[2]...
  mutate(node_index = as.numeric(gsub("[^0-9]", "", variable))) %>%
  arrange(node_index)

# Extract and immediately summarize
u_tidy <- fit %>%
  spread_draws(u[node_index]) %>%
  median_qi(.width = c(.95, .80)) # Calculates median and 80%/95% intervals

u_tidy %>%
  filter(node_index <= 20) %>%
  ggplot(aes(y = factor(node_index), x = u, xmin = .lower, xmax = .upper)) +
  geom_pointinterval() +
  labs(title = "Spatial Effects (u) by Node", x = "Effect Size", y = "Node Index")

smoothed_estimates <- fit %>%
  spread_draws(theta[node_index]) %>%
  median_qi(theta) %>%
  rename(REGION=node_index)

u_credible <- fit %>%
  spread_draws(theta[node_index]) %>%
  median_qi(theta, .width = 0.95) %>%
  # Create a flag for "significance" 
  # (where the interval doesn't cross 0 for log-odds)
  mutate(is_significant = .lower > 0 | .upper < 0) %>%
  rename(REGION=node_index) %>% 
  select(REGION,is_significant)

merged_shp_est <- left_join(merged_shp, smoothed_estimates, by = 'REGION')
merged_shp_est <- left_join(merged_shp_est, u_credible, by = 'REGION')

# Define breaks based on the quantiles you provided
# Calculate the mean of your exponentiated values


# 1. Prepare the data
plot_data <- merged_shp_est %>%
  mutate(
    # Set theta to NA for non-significant so it doesn't get color
    OR_sig = ifelse(is_significant, exp(theta), NA),
    # Define the pattern: "stripe" for non-sig, "none" for sig
    pattern_type = ifelse(is_significant, "none", "stripe"),
    # Label only significant nodes
    label_display = ifelse(is_significant, sprintf("%.2f", exp(theta)), "")
  )
ggplot(data = plot_data) +
  # Use geom_sf_pattern instead of geom_sf
  geom_sf_pattern(
    aes(
      fill = OR_sig, 
      pattern = pattern_type
    ),
    pattern_fill = "grey70",      # Color of the stripes
    pattern_color = "white",     # Background of the stripes
    pattern_spacing = 0.02,      # Density of the lines
    pattern_angle = 45,          # Diagonal lines
    color = "black",             # Border color of polygons
    size = 0.1
  ) +
  # Add labels for significant regions
  geom_sf_text(aes(label = label_display), size = 3, fontface = "bold") +
  # Set the manual scale for the pattern
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe"), guide = "none") +
  # Set the color gradient for the Odds Ratio
  scale_fill_gradient2(
    low = "#2c7bb6",   # Blue
    mid = "#ffffbf",   # Yellow
    high = "#d7191c",  # Red
    midpoint = 1.7,    
    limits = c(1.6, 2.3), 
    oob = scales::squish, 
    name = "Significant\nOR",
    na.value = "white" 
  ) +
  theme_bw() +
  theme(
    # Refined title and subtitle sizes
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "grey30"),
    plot.caption = element_text(size = 8, color = "grey50")
  ) +
  labs(
    title = "Unmet Need for Modern Contraception: Adolescent vs. Adult Women",
    subtitle = "Odds Ratios for Adolescents (<19 yrs) compared to Women 24-35 Years Old\nHatched lines indicate non-significant regions (95% CI includes 1.0)",
    caption = "Source: ENDESA 2019 | Model: BYM2"
  )
