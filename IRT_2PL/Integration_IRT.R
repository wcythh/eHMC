library(rstan)
library(mcmcse)
library(MASS)
library(transport)
library(parallel)

################################ data ################################ 
I0 <- 20
J0 <- 100
y0 <- 
  structure(c(1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0,
              0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1,
              1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0,
              1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1,
              1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1,
              1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1,
              1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1,
              0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1,
              1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1,
              1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
              0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1,
              1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1,
              0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0,
              1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1,
              0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1,
              1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1,
              0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1,
              1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1,
              0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1,
              1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0,
              1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1,
              1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0,
              1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0,
              1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0,
              1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1,
              1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1,
              0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1,
              1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1,
              1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0,
              0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1,
              1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1,
              0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1,
              1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1,
              0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1,
              1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0,
              0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0,
              0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
              0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1,
              0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0,
              1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
              1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1,
              1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1,
              0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1,
              0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1,
              0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1,
              1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,
              1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0,
              0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0,
              1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1,
              0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0,
              1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1,
              0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1,
              1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1,
              1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0,
              0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1,
              0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1,
              1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0,
              1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0,
              1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
              1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0,
              1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1,
              0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
              1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
              0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0,
              0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1,
              1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0,
              1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0,
              1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0,
              1, 0, 1, 0, 1, 0),
            .Dim = c(20, 100))
theta0 <- 
  c(-0.294127325104477, -0.243037835475601, -0.20729922215485,
    -0.945053335719786, -1.07451447431919, -1.43207175449777, -0.113110510882554,
    1.83302268194619, 0.297344608313486, 1.66677608010285, -1.00124777408435,
    -1.34997818025724, -0.726600549623881, 0.392151626122358, -2.06255979421154,
    -0.515998783025647, 0.823108436372549, 0.0237765792845014, 0.224484229991015,
    -1.18639769108632, 1.0001791836415, 1.31517361384902, -0.399303789319548,
    2.5856012312719, 2.13193450644519, 1.78592929573044, -0.236749450690456,
    0.74653121130132, 1.42535264816607, 0.203208783764513, -0.759157449522367,
    0.554425009644084, -1.05822806306339, -0.345518810419659, -1.42038634206988,
    -0.125825534716367, -1.24123888237234, 1.33206801088972, -0.186397459415001,
    -0.436772181876586, 0.694457076097829, -2.08033858814727, -0.239426894529614,
    -0.575797810994093, -0.0905168092829043, 1.15795907473804, 1.07574085522779,
    1.11673462740219, 0.177494960266088, 0.463772861541057, -0.777948946431177,
    -0.688390523996408, -0.198198231046454, -0.374201114995774, -1.44236559636931,
    0.872161873961532, 0.478263431754941, 0.154539947232711, 1.54523578500946,
    -0.498043089172587, -0.812813951132528, -1.14375046800425, 0.290682833870108,
    -1.05650208498398, -1.39850271836354, 1.09936520726879, -0.412762374120936,
    -0.122013437160995, -1.32797298350322, 0.0413625381003174, -0.0148299222740424,
    -2.45817192156614, -2.15361386189649, -0.577337022873291, 0.547628679681655,
    -0.473065458110804, -0.4054476799799, -1.66103005942605, -1.83354737692647,
    -0.524931399728034, -0.0129355006847102, -0.896164903215704, 1.81664449488324,
    0.730926161604017, 1.49172520298203, -1.87726490823095, -1.20505847616995,
    -0.349281435033805, -0.0249345156400613, -1.32357607711829, -0.971350835950955,
    0.479288429929995, -0.272825490112538, -0.104529779172503, -0.99660288930087,
    1.12300623164436, -0.915804254544829, 0.286075222966767, 0.0380626494381966,
    -1.46753779008444)
a0 <- 
  c(0.981148942898314, 1.28323704502609, 0.624510717693856,
    0.761269228317812, 1.30381507939538, 1.44472540665025, 0.978425706413301,
    0.479403788157739, 1.6929869999518, 0.868079703364671, 1.11583395861246,
    1.87694859019894, 0.961857815684477, 1.07343708115273, 1.53784887829854,
    1.23011245024635, 1.48079552706994, 1.6355356343192, 0.596370592272718,
    0.654558214836101)
b0 <- 
  c(-3.39211119177932, 1.73227160665065, -0.155456420985024,
    -0.118269501202021, -3.0015292453049, -2.16614181994718, -3.39278587267469,
    -1.32789251763701, -3.13431275999027, 1.8981740850377, -1.07701378052069,
    -2.09265534048495, 2.97367584164532, -0.396759469204122, -1.08223902058846,
    -0.818099286042566, -0.746818796114022, -1.63131741385355, -1.11523742276098,
    1.0396892119629)

MixSampler <- function(zz)
{
  set.seed(zz*1e7)
  
  ################################ NUTS ################################ 
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  d <- 144
  init0 <- c(log(1.0142e+00), theta0, log(5.0555e-01), log(a0), -9.4075e-01, log(2.1945e+00), b0)
  data = list(I=I0,J=J0,y=y0)
  iter0 <- 7e3
  warmup0 <- 2e3
  fit <- stan("irt_2pl_log.stan",
              data=data, chains=1, iter=iter0, warmup = warmup0,
              init = init0,save_dso = TRUE, verbose=FALSE,
              control = list(adapt_engaged=TRUE,stepsize=0.01, 
                             metric="unit_e",adapt_delta=0.25),
              algorithm = "NUTS")
  
  sampleNUTS <- as.matrix(fit)
  result <- fit@sim$samples
  L_trace <- attr(result[[1]],"sampler_params")$n_leapfrog__
  stepsize_trace <- attr(result[[1]],"sampler_params")$stepsize__
  accept_trace <- attr(result[[1]],"sampler_params")$accept_stat__
  depth_trace <- attr(result[[1]],"sampler_params")$treedepth__
  epsilon0 <- stepsize_trace[iter0]
  CompNUTS <- sum((2*L_trace[-c(1:warmup0)]+1))
  
  ################################ functions ################################ 
  U <- function(eta)
  {
    phi_theta <- eta[1]
    theta <- eta[2:101]
    phi_a <- eta[102]
    phi <- eta[103:122]
    a <- exp(phi)
    mu_b <- eta[123]
    phi_b <- eta[124]
    b <- eta[125:144]
    val <- 0
    for(i in 1:I0)
    {
      val0 <- a[i] * (theta-b[i])
      val <- val + sum(log(1+exp(val0))) - sum(y0[i,] * val0)
    }
    val <- val + log(1+0.25*exp(2*phi_theta)) - 
      phi_theta + sum(theta^2)/(2*exp(2*phi_theta)) +
      log(1+0.25*exp(2*phi_a)) - phi_a + sum(phi^2)/(2*exp(2*phi_a)) + mu_b^2/50 +
      log(1+0.25*exp(2*phi_b)) - phi_b + sum((b-mu_b)^2)/(2*exp(2*phi_b)) +
      J0*phi_theta + I0*phi_a + I0*phi_b
    return(val)
  }
  
  grad_U <- function(eta)
  {
    phi_theta <- eta[1]
    theta <- eta[2:101]
    phi_a <- eta[102]
    phi <- eta[103:122]
    a <- exp(phi)
    mu_b <- eta[123]
    phi_b <- eta[124]
    b <- eta[125:144]
    
    ## Compute grad phi_theta
    g_phi_theta <- 0.5/(0.25+exp(-2*phi_theta)) - 1 - sum(theta^2)/(exp(2*phi_theta)) + J0
    
    ## Compute grad theta_j
    g_theta <- rep(0,J0)
    for(j in 1:J0)
    {
      val0 <- a *(theta[j] - b)
      g_theta[j] <- sum(a/(1+exp(-val0))) - sum(y0[,j] * a) + theta[j]/(exp(2*phi_theta))
    }
    
    ## Compute grad phi_a
    g_phi_a <- 0.5/(0.25+exp(-2*phi_a)) - 1 - sum(phi^2)/exp(2*phi_a) + I0
    
    ## Compute grad phi_i
    g_phi <- rep(0,I0)
    for(i in 1:I0)
    {
      val0 <- a[i] * (theta - b[i])
      g_phi[i] <- sum(val0/(1+exp(-val0))) - sum(y0[i,]*val0) + phi[i]/(exp(2*phi_a))
    }
    ## Compute grad mu_b
    g_mu_b <- mu_b/25 - sum(b-mu_b)/exp(2*phi_b)
    
    ## Compute grad phi_b
    g_phi_b <- 0.5/(0.25+exp(-2*phi_b)) - 1 - sum((b-mu_b)^2)/exp(2*phi_b) + I0
    
    ## Compute grad b_i
    g_b <- rep(0,I0)
    for(i in 1:I0)
    {
      val0 <- a[i] *(theta - b[i])
      g_b[i] <- sum(-a[i]/(1+exp(-val0))) + sum(y0[i,])*a[i] + (b[i]-mu_b)/exp(2*phi_b)
    }
    
    return(c(g_phi_theta, g_theta, g_phi_a, g_phi, g_mu_b, g_phi_b, g_b))
  }
  ################################ Computing longest batches ################################
  HMC1 <- function(U,grad_U,epsilon,L,x)
  {
    current_x <- x
    current_v <- rnorm(length(x))
    
    proposed_x <- current_x
    proposed_v <- current_v
    
    candidate_x <- current_x
    candidate_v <- current_v
    
    Increment <- 0
    Stop <- 0
    StopInd <- 0
    ell <- 0
    
    while(Increment >= 0 | ell < L)
    {
      ell <- ell + 1
      proposed_v <- proposed_v - 0.5*epsilon*grad_U(proposed_x)
      proposed_x <- proposed_x + epsilon*proposed_v
      proposed_v <- proposed_v - 0.5*epsilon*grad_U(proposed_x)
      if(Stop == 0)
      {
        Delta_x <- proposed_x - current_x
        Increment <- sum(Delta_x * proposed_v)
        if(is.na(Increment))
        {
          return(c(current_x, 0, NA, NA, 0))
        }
        if(Increment < 0)
        {
          StopInd <- ell
          Stop <- 1
        }
      }
      if(ell == L)
      {
        candidate_x <- proposed_x
        candidate_v <- proposed_v
      }
    }
    
    current_U <- U(current_x)
    current_K <- 0.5*sum(current_v^2)
    candidate_U <- U(candidate_x)
    candidate_K <- 0.5*sum(candidate_v^2)
    
    rho <- current_U + current_K - candidate_U - candidate_K
    if(is.na(rho))
    {
      return(c(current_x, 0, NA, NA, 0))
    }
    else
    {
      u <- log(runif(1))
      if(u < rho)
      {
        return(c(candidate_x, 1, rho, StopInd, ell))
      }
      else
      {
        return(c(current_x, 0, rho, StopInd, ell))
      }
    }
  }
  ################################ Classical HMC Sampler ################################
  HMC <- function(U,grad_U,epsilon,L,current_q)
  {
    q = current_q
    p = rnorm(length(q),0,1)
    current_p = p
    p = p - 0.5*epsilon * as.vector(grad_U(q))
    for(l in 1:L)
    {
      q = q + epsilon * p
      if(l != L)
        p = p - epsilon * as.vector(grad_U(q))
    }
    p = p - 0.5*epsilon * grad_U(q)
    p = -p
    current_U <- U(current_q)
    current_K <- sum(current_p^2)/2
    proposed_U <- U(q)
    proposed_K <- sum(p^2)/2
    rho <- exp(current_U-proposed_U+current_K-proposed_K)
    if(is.na(rho))
    {
      return(c(current_q,0,NA))
    }
    else
    {
      if(runif(1) < rho)
      {
        return(c(q,1,rho))
      }
      else
      {
        return(c(current_q,0,rho))
      }
    }
  }
  
  ################################ ChooseL ################################ 
  ChooseL <- function(epsilon, theta, Niter)
  {
    Xsim <- matrix(0,nrow=Niter,ncol=d)
    Xsim[1,] <- theta
    #print(Xsim[1,])
    L <- 50
    Ind_trace <- L
    ratio <- 0
    Comp <- 0
    Comp_Random <- 0
    ratio_theory <- 0
    time_L <- proc.time()
    i <- 1
    while(i < Niter)
    {
      L <- quantile(Ind_trace,0.95)
      L_r <- sample(1:L,size=1)
      Xstar <- HMC1(U,grad_U,epsilon,L_r,Xsim[i,])
      if(!is.na(Xstar[d+2]))
      {
        i <- i+1
        Xsim[i,] <- Xstar[1:d]
        ratio <- ratio + Xstar[d+1]
        ratio_theory <- ratio_theory + exp(min(c(0,Xstar[(d+2)])))
        Ind_trace <- c(Ind_trace, Xstar[d+3])
        Comp <- Comp + 2*Xstar[(d+4)] + 1
        Comp_Random <- Comp_Random + 2*L_r+1
        #print(round(c(i,Xsim[i,1:d],Ind_trace[length(Ind_trace)],length(Ind_trace),L_r),digits=4))
      }
      if(i %% floor(Niter/10) == 0)
        print(paste("Iteration ",toString(100*round(i/Niter,digits = 2)), "%",sep=""))
    }
    time_L <- proc.time() - time_L
    Ind_trace <- Ind_trace[which(!is.na(Ind_trace))]
    print(round(c(ratio/Niter,length(Ind_trace),time_L),digits = 4))
    
    return(list(epsilon=epsilon, Niter=Niter, Ind_trace=Ind_trace,
                Xsim=Xsim, time=time_L, ratio=ratio/Niter, ratio_theory=ratio_theory/Niter))
  }
  ################################ eHMCq ################################
  eHMCq <- function(Ind_trace,Niter,epsilon,theta)
  {
    L <- as.numeric(floor(quantile(Ind_trace, 0.95)))
    #Niter_hmc <- floor(CompNUTS/(0.5*L+2.5))
    Niter_hmc <- Niter
    print(c(L, Niter_hmc))
    Xsim <- matrix(0,nrow=Niter_hmc,ncol=d)
    Xsim[1,] <- theta
    
    Comp <- 0
    ratio <- rep(0,Niter_hmc)
    ratio_theory <- rep(0,Niter_hmc)
    time_HMC <- proc.time()
    i <- 1
    while(i < Niter_hmc)
    {
      r <- sample(1:L,size=1)
      Xstar <- HMC(U,grad_U,epsilon,r,Xsim[i,])
      if(!is.na(Xstar[d+2]))
      {
        i <- i + 1
        Xsim[i,] <- Xstar[1:d]
        ratio[i] <- Xstar[d+1]
        ratio_theory[i] <- Xstar[d+2]
        Comp <- Comp + r + 2
      }
      if(i %% floor(Niter_hmc/10) == 0)
        print(paste("Iteration ",toString(100*round(i/Niter_hmc,digits = 2)), "%",sep=""))
    }
    time_HMC <- proc.time() - time_HMC
    print(c(sum(ratio)/Niter_hmc,time_HMC))
    
    sampleRandom <- cbind(Xsim, exp(Xsim[,1]),exp(Xsim[,102]),exp(Xsim[,124]),
                          exp(Xsim[,103:122]),-apply(Xsim,1,U))
    
    colnames(sampleRandom) <- colnames(sampleNUTS)
    
    return(list(sampleRandom=sampleRandom, L=L, Comp=Comp, epsilon=epsilon,Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  ################################ eHMC ################################
  eHMC <- function(Ind_trace,Niter,epsilon,theta)
  {
      #Niter_hmc <- floor(CompNUTS/(mean(Ind_trace)+2))
    Niter_hmc <- Niter
    print(Niter_hmc)
    Xsim <- matrix(0,nrow=Niter_hmc,ncol=d)
    Xsim[1,] <- theta
    
    Comp <- 0
    ratio <- rep(0,Niter_hmc)
    ratio_theory <- rep(0,Niter_hmc)
    time_HMC <- proc.time()
    i <- 1
    while(i < Niter_hmc)
    {
      #r <- sample(1:L,size=1)
      r <- sample(Ind_trace,size=1)
      Xstar <- HMC(U,grad_U,epsilon,r,Xsim[i,])
      if(!is.na(Xstar[d+2]))
      {
        i <- i + 1
        Xsim[i,] <- Xstar[1:d]
        ratio[i] <- Xstar[d+1]
        ratio_theory[i] <- Xstar[d+2]
        Comp <- Comp + r + 2
      }
      if(i %% floor(Niter_hmc/10) == 0)
        print(paste("Iteration ",toString(100*round(i/Niter_hmc,digits = 2)), "%",sep=""))
    }
    time_HMC <- proc.time() - time_HMC
    print(c(sum(ratio)/Niter_hmc,time_HMC))
    
    sampleRandom <- cbind(Xsim, exp(Xsim[,1]),exp(Xsim[,102]),exp(Xsim[,124]),
                          exp(Xsim[,103:122]),-apply(Xsim,1,U))
    
    colnames(sampleRandom) <- colnames(sampleNUTS)
    
    return(list(sampleRandom=sampleRandom, Comp=Comp, epsilon=epsilon, Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  ################################ eHMCu ################################
  eHMCu <- function(Ind_trace,Niter,epsilon,theta)
  {
      #Niter_hmc <- floor(CompNUTS/(0.5*mean(Ind_trace)+2.5))
    Niter_hmc <- Niter
    print(Niter_hmc)
    Xsim <- matrix(0,nrow=Niter_hmc,ncol=d)
    Xsim[1,] <- theta
    
    Comp <- 0
    ratio <- rep(0,Niter_hmc)
    ratio_theory <- rep(0,Niter_hmc)
    time_HMC <- proc.time()
    i <- 1
    while(i < Niter_hmc)
    {
      L <- sample(Ind_trace,size=1)
      r <- sample(1:L,size=1)
      Xstar <- HMC(U,grad_U,epsilon,r,Xsim[i,])
      if(!is.na(Xstar[d+2]))
      {
        i <- i + 1
        Xsim[i,] <- Xstar[1:d]
        ratio[i] <- Xstar[d+1]
        ratio_theory[i] <- Xstar[d+2]
        Comp <- Comp + r + 2
      }
      if(i %% floor(Niter_hmc/10) == 0)
        print(paste("Iteration ",toString(100*round(i/Niter_hmc,digits = 2)), "%",sep=""))
    }
    time_HMC <- proc.time() - time_HMC
    print(c(sum(ratio)/Niter_hmc,time_HMC))
    
    sampleRandom <- cbind(Xsim, exp(Xsim[,1]),exp(Xsim[,102]),exp(Xsim[,124]),
                          exp(Xsim[,103:122]),-apply(Xsim,1,U))
    
    colnames(sampleRandom) <- colnames(sampleNUTS)
    
    return(list(sampleRandom=sampleRandom, Comp=Comp, epsilon=epsilon, Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  
  #fit0 <- ChooseL(epsilon0, sampleNUTS[1,1:d], 5e3)
  #hist(fit0$Ind_trace,breaks = 50)
  #quantile(Ind_trace,0.95)
  #fit1 <- Sampler1(fit0$Ind_trace, CompNUTS, epsilon0, sampleNUTS[1,1:d])
  #fit2 <- Sampler2(fit0$Ind_trace, CompNUTS, epsilon0, sampleNUTS[1,1:d])
  #fit3 <- Sampler3(fit0$Ind_trace, CompNUTS, epsilon0, sampleNUTS[1,1:d])
  
  fit0 <- ChooseL(epsilon0,init0,2e3)
  #hist(fit0$Ind_trace,breaks=50)
  #plot(fit0$Ind_trace,type="l")
  fit1 <- eHMCq(fit0$Ind_trace, CompNUTS, epsilon0, fit0$Xsim[fit0$Niter,])
  fit2 <- eHMC(fit0$Ind_trace, CompNUTS, epsilon0, fit0$Xsim[fit0$Niter,])
  fit3 <- eHMCu(fit0$Ind_trace, CompNUTS, epsilon0, fit0$Xsim[fit0$Niter,])
  
  return(list(fitNUTS=fit, fit0=fit0,fit1=fit1, fit2=fit2, fit3=fit3))
}

num_cores <- detectCores()

cl <- makeCluster(num_cores,type = "FORK")

################################ repeat 40 times ################################ 
Result <- parLapply(cl, 1:40, function(zz) MixSampler(zz))

save(Result, file="Result.RData")
stopCluster(cl)

result1 <- Result[[1]]
result2 <- Result[[2]]

length(result1)

