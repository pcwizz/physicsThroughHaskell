--PV=nRT=kNT
module ThermalPhysicsForAnIdealGas where
	avogadro = 6.02e23 -- N_a
	boltzman = 1.38e-23 -- k
	molarGas = avogadro * boltzman -- R
	pressureFromForceAndArea :: Double -> Double -> Double
	pressureFromForceAndArea f a = f/a -- Force divided by area
	e :: Double -> Double -> Double -> Double
	e m c t = m*c*t -- E=mcT change in energy is mass by specific thermal capacity by change in temperature
	changeInT  :: Double -> Double -> Double
	changeInT t_start t_end = t_end - t_start -- change in temperature
	c_rms  :: Double -> Double
	c_rms c = sqrt (c^2) -- Root mean square velocity gives a speed.
	keneticEFromMassAndc  :: Double -> Double -> Double
	keneticEFromMassAndc m c = (1/2)*m*(c_rms c)^2 -- E_k=1/2mc^2 lame Newtonian mechanics
	keneticEFromTempreture :: Double -> Double -> Double
	keneticEFromTempreture t k = (3/2)*k*t -- E_k=3/2kT
	molesFromDouble  :: Double -> Double
	molesFromDouble n = n / avogadro -- get moles from the number of particles
	molarMassFromMass  :: Double -> Double
	molarMassFromMass m = m / avogadro -- Get molar mass from the mass
	numberFromMoles  :: Double -> Double
	numberFromMoles n = n * avogadro -- Get the real number of particles from the moles
	massFromMolarMass  :: Double -> Double
	massFromMolarMass m = m * avogadro -- Get the real mass from the molar mass
	celsiusToKelvin :: Double -> Double
	celsiusToKelvin t = t + pointZero -- converts temperatures in Celsius to kelvin
		where
		pointZero = 273
	pressure_nRT :: Double -> Double -> Double -> Double
	pressure_nRT n t v = (n*molarGas*t)/v -- Pressure from moles
	presure_NkT :: Double -> Double -> Double -> Double
	presure_NkT n t v = (n*boltzman*t)/v -- Pressure from actual number
	volume_nRT :: Double -> Double -> Double -> Double
	volume_nRT n t p = (n*molarGas*t)/p -- volume from moles
	volume_NkT :: Double -> Double -> Double -> Double
	volume_NkT n t p = (n*boltzman*t)/p -- volume from actual number
	area :: Double -> Double -> Double
	area x y = x*y -- basic area calculation
	volume_xyz :: Double -> Double -> Double -> Double
	volume_xyz x y z = (area x y) * z -- work out a volume simply with dimensions 
	specificThermalcapacity :: Double -> Double -> Double -> Double
	specificThermalcapacity e m t = e/(m * t) -- Work out the specific thermal capacity
	meanEnergyBoltzmanDistribution :: Double -> Double
	meanEnergyBoltzmanDistribution t = t * boltzman -- approx mean energy of a particle
