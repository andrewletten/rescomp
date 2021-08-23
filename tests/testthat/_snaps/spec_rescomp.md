# Correct model messages

    Code
      spec_rescomp()
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(spnum = 2)
    Message <simpleMessage>
      Model properties 
       * 2 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10, 10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      [2,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      [2,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      [2,] 0.001
      
      $phi
           [,1]
      [1,]    0
      [2,]    0
      
      $type3
           [,1]
      [1,]  0.5
      [2,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 2
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(resnum = 2)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 2 resource(s)
       * Consumers have type 1 functional responses
       * Resources are substitutable
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1, 1]
    Output
      $mu
      $mu[[1]]
           [,1] [,2]
      [1,]  0.1  0.1
      
      
      $Ks
           [,1] [,2]
      [1,]    1    1
      
      $Qs
            [,1]  [,2]
      [1,] 0.001 0.001
      
      $phi
           [,1] [,2]
      [1,]    0    0
      
      $type3
           [,1] [,2]
      [1,]  0.5  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 2
      
      $resspeed
      [1] 0.03 0.03
      
      $resconc
      [1] 1 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(funcresp = "type2")
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 2 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    1
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(essential = TRUE)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] TRUE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = TRUE)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = TRUE, respulse = 1)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat) AND pulsed
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Resources pulsing every 0 timesteps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 1
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = TRUE, resspeed = 0, respulse = 1)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is pulsed only
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Resources pulsing every 0 timesteps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 1
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = FALSE, resspeed = 0, respulse = 1)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resources are pulsed only
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Resources pulsing every 0 timesteps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 1
      
      $essential
      [1] FALSE
      
      $chemo
      [1] FALSE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = FALSE, resspeed = 0, respulse = 0)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resources are not supplied?!
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] FALSE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = FALSE, resspeed = 1, respulse = 0)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resources grow logistically
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] FALSE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 1
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = TRUE, resspeed = 0, respulse = 0)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resources are not supplied?!
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = TRUE, resspeed = 1, respulse = 0)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 1
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(chemo = FALSE, resspeed = 1, respulse = 1)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resources grow logistically and are pulsed
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Resources pulsing every 0 timesteps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 1
      
      $essential
      [1] FALSE
      
      $chemo
      [1] FALSE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 1
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(batchtrans = TRUE)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] TRUE
      

---

    Code
      spec_rescomp(mortpulse = 0.5)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous and intermittent
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Intermittent mortality every 0 timesteps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0.5
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(mortpulse = 0.5, respulse = 1)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat) AND pulsed
       * Mortality is continuous and intermittent
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Resources pulsing and intermittent mortality every 0 timesteps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0.03
      
      $respulse
      [1] 1
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0.5
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(mortpulse = 0.5, mort = 0)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality intermittent
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Intermittent mortality every 0 timesteps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0.5
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

---

    Code
      spec_rescomp(mort = 0)
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * No mortality
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]
    Output
      $mu
      $mu[[1]]
           [,1]
      [1,]  0.1
      
      
      $Ks
           [,1]
      [1,]    1
      
      $Qs
            [,1]
      [1,] 0.001
      
      $phi
           [,1]
      [1,]    0
      
      $type3
           [,1]
      [1,]  0.5
      
      $cinit
      [1] 10
      
      $all_d
      [1] 0
      
      $respulse
      [1] 0
      
      $essential
      [1] FALSE
      
      $chemo
      [1] TRUE
      
      $nconsumers
      [1] 1
      
      $nresources
      [1] 1
      
      $resspeed
      [1] 0.03
      
      $resconc
      [1] 1
      
      $timepars
      [1] FALSE
      
      $mortpulse
      [1] 0
      
      $totaltime
      [1] 1000
      
      $pulsefreq
      [1] 0
      
      $batchtrans
      [1] FALSE
      

