--PV=nRT=kNT
module ThermalPhysicsForAnIdealGas where
	let avogadro = 6.02 *(10^23) -- N_a
	let	boltzman = 1.38 *(10^(-23)) -- k
	let molarGas = avogadro * boltzman -- R
	pressureFromForceAndArea :: Number -> Number -> Number
	pressureFromForceAndArea f a = f/a -- Force divided by area
	e :: Number -> Number -> Number -> Number
	e m c t = m*c*t -- E=mcT change in energy is mass by specific thermal capacity by change in temperature
	changeInT  :: Number -> Number -> Number
	changeInT t_start t_end = t_end - t_start -- change in temperature
	c_rms  :: Number -> Number
	c_rms c = sqrt (c^2) -- Root mean square velocity gives a speed.
	keneticEFromMassAndc  :: Number -> Number -> Number
	keneticEFromMassAndc m c = (1/2)*m*(c_rms c)^2 -- E_k=1/2mc^2 lame Newtonian mechanics
	keneticEFromTempreture :: Number -> Number -> Number
	keneticEFromTempreture t k = (3/2)*k*t -- E_k=3/2kT
	molesFromNumber  :: Number -> Number
	molesFromNumber n = n / avogadro -- get moles from the number of particles
	molarMassFromMass  :: Number -> Number
	molarMassFromMass m = m / avogadro -- Get molar mass from the mass
	numberFromMoles  :: Number -> Number
	numberFromMoles n = n * avogadro -- Get the real number of particles from the moles
	massFromMolarMass  :: Number -> Number
	massFromMolarMass m = m * avogadro -- Get the real mass from the molar mass
	celsiusToKelvin :: Number -> Number
	celsiusToKelvin t = t + pointZero -- converts temperatures in Celsius to kelvin
		where
		pointZero = 273
	pressure_nRT :: Number -> Number -> Number -> Number
	pressure_nRT n t v = (n*molarGas*t)/v -- Pressure from moles
	presure_NkT :: Number -> Number -> Number -> Number
	presure_NkT n t v = (n*boltzman*t)/v -- Pressure from actual number
	volume_nRT :: Number -> Number -> Number -> Number
	volume_nRT n t p = (n*molarGas*t)/p -- volume from moles
	volume_NkT :: Number -> Number -> Number -> Number
	volume_NkT n t p = (n*boltzman*t)/p -- volume from actual number
	area :: Number -> Number -> Number
	area x y = x*y -- basic area calculation
	volume_xyz :: Number -> Number -> Number -> Number
	volume_xyz x y z = (area x y) * z -- work out a volume simply with dimensions 
	specificThermalcapacity :: Number -> Number -> Number -> Number
	specificThermalcapacity e m t = e/(m * t) -- Work out the specific thermal capacity
	meanEnergyBoltzmanDistribution :: Number -> Number
	meanEnergyBoltzmanDistribution t = t * boltzman -- approx mean energy of a particle
