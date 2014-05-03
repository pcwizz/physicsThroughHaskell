{-
The MIT License (MIT)

Copyright (c) 2014 Morgan Hill

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}

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
	molesFromNumber  :: Double -> Double
	molesFromNumber n = n / avogadro -- get moles from the number of particles
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
