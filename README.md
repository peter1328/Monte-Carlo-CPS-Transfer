##Monte Carlo Simulations for Power Estimation

A Monte Carlo simulation study was conducted for power estimation in the software Mplus (Muthén & Muthén, 2014). An introduction to Monte Carlo power analysis in Mplus is provided in Muthen and Muthen (2002). A discussion of  the specification of intervention effects is given in Thoemmes, MacKinnon, and Reiser (2010).

###Aim of the Simulation Study
The aim of the simulation study was to estimate the power (probability of finding a significant effect) for detecting intervention effects of the Experimentation Skills Training and the MINT Curriculum. It should be evaluated at which smaple size the threshold for adequate power of >.80 is reached, and which power is reached with the maximum planned sample size of N = 1200.

###Basic Specifications for the Simulations
The simulations were run for sample sizes between N = 500 and N = 1200, increasing stepwise by n = 100. Each simulation was based on 10000 draws for the respective sample size. Power was estimated as the average of the two percentages of significant parameter estimates for the intervention effects of the Experimentation Skills Training and the MINT curriculum in the 10000 draws.

###Simulated Structural Equation Models
A two-level regression model was simulated with school classes as level two clusters (see e.g., Raudnbush & Bryk, 2002). For the population model from which the Monte Carlo samples were drawn, a regression of the posttest (e.g., Complex Solving Test, Experimentation Skills test, or Experimentation Skills Interview after the two interventions) on the pretest (the same respective test before the Experimentation Skills Training) was specified. The pretest served as predictor of the posttest on both levels of the analysis, that is, on the level of the individual student, and on the level of the school class (as class average). Crucially, on the between level the posttest was additionally regressed on the two intervention variables representing the effect of the Experimentation Skills Training (est) and of the MINT curriculum (mint).

###Assumption for the Simulations
Apart from the twolevel regression model, all assumptions were chosen in a conservative manner as follows:
- Values of the dependent variable (posttest) were set to be missing with a probability of 10% (5% was estimated to be realistically reachable, hence 10% is to be seen as a conservative estimate)
- In accordance with data of 300 school classes form the Swiss MINT Study, classroom sizes were set as follows: For the sample of 700 students, 40 classrooms were assumed to be assessed, with an average number of 20 students per class and a range of 10 - 30 students per class. Extreme classroom sizes were assumed to be less frequent than average classroom sizes.
- As an estimate of retest-reliability, it was assumed that on the within-level the pretest can account for about 80% of the variance in the posttest.
- On the between level, it was assumed that the pretest can account for 80% of between-classroom differences at posttest
- On the between level, each of the interventions accounts for 5% (representing an additive transfer effect of 10% explained variance, the assumption was 20% but half as much was chosen as a conservative estimate)
- The rest of 10% of between-classroom variance cannot be explained and stays as residual variance (5% was estimated to be realistic, hence 10% again represents a conservative specification)

##Results
Esimated power to detect the intervention effects in dependence of sample size is depicted in Figure 1. The threshold of power >.80 was reached at a sample size of N = 700. At the maximum planned sample size of 1200, power was estimated at .96. These results confirm that a sample size of between 700 and 1200 students can be seen optimal to detect small to moderate transfer effects with sufficiently high but not too high certainty and an optimal balance of alpha and beta error risk can be reached.

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 


##References
Muthén, B., & Muthen, L. (2002). How to decide on smaple size using Monte Carlo Simulations in Mplus. Structurla Equation Modeling, 2, 599-638.
Muthén, B., & Muthen, L. (2014). Mplus User's Guide. Brooklyn, NY.
Thoemmes, MacKinnon, and Reiser (2010). Power Analysis for Complex Mediational Models in Mplus. Structurla Equation Modeling, 12, 99-128.


##Appendix

##Commented Mplus sample input syntax file for N = 700
Following, the syntax of the Mplus input for the Monte Carlo simulation with a sample size of N = 700 students form 40 school classes is given. All other input files and also output files can be found in the root (folder) "Mplus_input_output".


TITLE:  Monte Carlo Simulation
        for Transfer Effect
        of Intervention on Complex Problem Solving or other transfer measures
Assumptions:
- Intraclass correlation coefficient about .10:
  The variance between classrooms makes up
  about 10% of the whole variance after the intervention
  (about 8% before intervention,
   1% due to intervention,
   1% due to non-controllable factors)
- Posttest variance within classrooms:
  80% explained by pretest (retest-reliability)
  20% left unexplained
- Posttest variance between classrooms:
  about 80% explained by pretest
  about 5% explained by est: Experimentaion Skills Training
  about 5% explained by mint: MINT Curriculum
  about 10% left unexplained due to non-controllable factors
- 10% Missing data on posttest
- Class sizes vary from 10 t0 30 children per class,
  with more classes in the middle range (15-25 children)

montecarlo:
        ! Define simulated variables
  		names are 
        post ! Complex Problem Solving posttest
        pre  ! Compelx Problem Solving pretest
        est  ! Intervention 1: Expeirmentation Skills Training
        mint;! Intervention 2: MINT curriculum
  		nobservations = 700; ! Total sample size for each Monte Carlo-draw
  		ncsizes = 5; ! Nubmer of different classroom sizes
        ! Classroom sample: Overall 35 Classrooms with 10 - 30 children
  		csizes = 4 (10) 8 (15)  11 (20) 8 (25)  4 (30); ! Number and size of the 35 classrooms
        ! Define intervention variable (>.5 = intervention group;
        ! see Thoemmes, MacKinnon, & Reiser, 2010)
        cutpoints = est (.5) mint (.5);
        ! Define missing values
        PATMISS = post (.10); ! 10% of posttest-data on CPS are missing
  		PATPROBS = 1;
        ! Define seed for quasi-random number generation
        seed = 666;
        ! Define number of Monte Carlo draws from the population
  		nreps = 10000;
        ! Define variables on second level (school class level)
          between = est mint; ! The interventions happen on the classroom level
        !save = MCTransfer_*.dat; ! Activate to save sampled data
        !repsave = all;         ! Activate to save sampled data

ANALYSIS:   TYPE = TWOLEVEL;

model population:
		
        %within%
        ! Posttest-residual variance
        post*2; ! Retest-reliability .80 20% variance not explained by pre-test
        pre*10; ! Pretest-variance within classrooms
        post ON pre*.894; ! Pretest explains 80% (.894^2) of posttest       

        %between%
        post*.10; ! 10% of posttest-variance unexplained
        pre*1; ! Pretest-variance between classrooms
        [est*.5]; ! Mean of "est" intervention variable
        ! Variance of "est" intervention variable
        est*.25; ! .25 so that SD = .5
        [mint*.5]; ! Mean of "mint" intervention variable
        ! Variance of "mint" intervention variable
        mint*.25; ! .25 so that SD = .5
        ! Regression of posttest variance between classrooms
        post ON pre*.949 est*.447 mint*.447; ! .90 explained by pretest (.949^2*1),
                                             ! .05 by est (.447^2*.25),
                                             ! .05 by mint (.447^2*.25)
                                             ! = 1.00 explained variance,
                                             ! .10 non-explained,
                                             ! sums up to variance of 1.10 at posttest
                                             ! which makes up an ICC of about .10
        		
model:
        ! Same as population model without mentioning
        ! means and variances of exogenous variables
        %within%
        post*2;

        post ON pre*.894;

        %between%
        post*.10;



        post ON pre*.949 est*.447 mint*.447;



output:
		tech9; ! Show simulation results
