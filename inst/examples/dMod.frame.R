
\dontrun{

# ---- Example

setwd(tempdir())  
  
## Part 1: Defining basic object ----------------------------------------

# Define simple ODE model
f <- NULL |>
  addReaction("A", "B", "k1*A", "translation") |> 
  addReaction("B",  "", "k2*B", "degradation") |> 
  as.eqnvec()

# Add value to state A at time 5
events <- eventlist(var = "A", time = 5, value = "A_add", method = "add")
# Prediction function
x <- odemodel(f, events = events) |> Xs
# Observation function
g <- Y(c(Bobs = "s1*B"), x, compile = T, modelname = "obsfn")


# Define parameters for two conditions 
conditions <- c("a", "b")
parameters <- union(getParameters(g), getParameters(x))

trafo <-
  NULL |>
  define("x~x", x = parameters) |>
  branch(conditions = conditions) |>
  insert("x~x_cond", x = "s1", cond = condition) |>
  insert("x~1", x = "A_add", conditionMatch = "a") |>
  insert("x~5", x = "A_add", conditionMatch = "b") |>
  insert("x~exp(x)", x = getSymbols(mytrafo[[i]]))

p <- P(trafo)

## Process data

# Data
data <- datalist(
  a = data.frame(time = c(0, 2, 7),
                 name = "Bobs",
                 value = c(.3, .3, .3),
                 sigma = c(.03, .03, .03)),
  b = data.frame(time = c(0, 2, 7),
                 name = "Bobs",
                 value = c(.1, .1, .2),
                 sigma = c(.01, .01, .02))
)

## Part 2: dMod.frame -------------------------------------------------------

# construct dMod.frame
myframe1 <- dMod.frame("no steady states", g = g, x = x, p = p, data = data)
print(myframe1)


# Augment by derived objects: prd, obj_data, obj, times, pars
set.seed(4)
myframe2 <- myframe1 |> appendObj(
  # Typical prediction function
  prd = list(g*x*p), 
  # Typical objective function with predifined error (sigma) on the data
  obj_data = list(normL2(data, prd)), 
  # Simulation times
  times = list(seq(0, 10, length.out = 100))
)
print(myframe2)


# Plot the model with random pars
plotCombined(myframe2)

# Add prior
myframe3 <- myframe2 |> mutate(
  constr = list(constraintL2(mu = 0*pars, sigma = 5)),
  obj = list(obj_data + constr)
)

# Run fits
myframe4 <- myframe3 |> mutate(
  fits = list(mstrust(obj, pars, studyname = "Fits", fits = 20, cores = 4, blather = T))
)


# Extract parameters from list of fits (parframe)
myframe5 <- myframe4 |> appendParframes(
  parframes = list(as.parframe(fits))
)

# Visualize
# Little Bug: If you want to use the dots that go to subset(), you need to specify the others
plotCombined(myframe5, hypothesis = 1, index = 1, F, grepl("B", name))

plotPars(myframe5)

plotValues(myframe5)

plotValues(myframe5, 1, tol =0.0000000001)



# Add Profiles
myframe6 <- myframe5 |> mutate(
  profiles = list(profile(obj, as.parvec(parframes), whichPar = "k1"))
)

myframe6$profiles |> plotProfile()


# Validation profile

myframe7 <- myframe6 |> mutate(
  vali = list(datapointL2("A", 2, "mypoint", .1, condition = "a")),
  obj_vali = list(obj_data + constr + vali),
  par_vali = list(c(dModLib:::sanitizePars(as.parvec(parframes))$pars, "mypoint" = 0.1 )),
  fits_vali = list(mstrust(obj_vali, par_vali)),
  profile_vali = list(profile(obj_vali, fits_vali |> as.parframe() |> as.parvec(), "mypoint"))
)

myframe7$profile_vali |> plotProfile()




## Part 3:Easily test several hypotheses --------------------------------------------

# Fit with various prior strengths 
multiframe <- 
  dMod.frame("no steady states", g, x, p, data) |> 
  appendObj(
    prd = list(g*x*p), 
    obj_data = list(normL2(data, prd)), 
    times = list(seq(0, 10, length.out = 100))
  ) %>% 
  # replicate four times
  rbind(.,.,.,.) |> 
  # If you don't ungroup and run a mutate with an "lapply(1:nrow(), function(i) ...", 
  # the index i always gets restored to 1, as mutate() runs the 
  # expressions (...) independently for each group.
  ungroup() |> 
  mutate(
    constr = purrr::map(seq_along(x), function(i) {
      constraintL2(mu = 0*pars[[i]], sigma = 10^(i-3))
    }),
    hypothesis = purrr::map_chr(seq_along(x), function(i) {
      paste0(hypothesis[[i]], ", prior sigma = ", 10^(i-3))
    })
  ) |> 
  rowwise() |> #regroup by row for convenient interface to mutate()
  mutate(
    obj = list(obj_data + constr),
    fits = list(mstrust(obj, pars, studyname = "Fits", fits = 10, cores = 4, blather = T))
  ) |> 
  appendParframes(parframes = list(as.parframe(fits)))
  

# Plot
purrr::map(1:4, function(i) multiframe |> plotValues(hypothesis = i))

purrr::map(1:4, function(i) multiframe |> plotCombined(hypothesis = i))

# Profiles
multiframe <- multiframe |> mutate(
  profiles = list(profile(obj, parframes |> as.parvec(), names(pars), cores = 4))
)

# Plot profiles: The "profiles"-column is already a proflist :)
multiframe$profiles |> plotProfile() +
  coord_cartesian(ylim = c(-0.5, 4), xlim = c(-2,2))



# Quick and dirty analysis of one single hypothesis ----
checkout_hypothesis(multiframe, 4, suffix = "_weak_prior")
parframes_weak_prior


}
