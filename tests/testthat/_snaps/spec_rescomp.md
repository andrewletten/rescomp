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

---

    Code
      spec_rescomp(timepars = TRUE, mumatrix = list(matrix(1), matrix(1)),
      timeparfreq = 100, tpinterp = "inst")
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
       * Time dependent parameters with instantaneous switching every 100 timesteps
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]

---

    Code
      spec_rescomp(timepars = TRUE, mumatrix = list(matrix(1), matrix(1)),
      timeparfreq = 100, tpinterp = "lin")
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
       * Time dependent parameters with linear interpolation (period = 200 timesteps)
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]

---

    Code
      spec_rescomp(timepars = TRUE, mumatrix = list(matrix(1), matrix(1)),
      timeparfreq = 100, tpinterp = "sine")
    Message <simpleMessage>
      Model properties 
       * 1 consumer(s) and 1 resource(s)
       * Consumers have type 1 functional responses
       * Resource supply is continuous (e.g. chemostat)
       * Mortality is continuous
       * Time dependent parameters with sinusoidal interpolation (period = 200 timesteps)
      
      Simulation properties 
       * Simulation time: 1000 time steps
       * Init state: consumer(s) = [10], resource(s) = [1]

