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
module Mechanics where
	-- Equations for motion with constant acceleration (SUVAT)
	finalVelocity_uat :: Double -> Double -> Double -> Double
	finalVelocity_uat u a t = u + a * t -- v=u+at
	finalVelocity_uas :: Double -> Double -> Double -> Double
	finalVelocity_uas u a s = sqrt (u^2 + 2*a*s) -- v^2=u^2 + 2as
	displacement_uat :: Double -> Double -> Double -> Double
	displacement_uat u a t = u*t + 1/2 * a * t^2 -- s=ut+at^2
	displacement_vat :: Double -> Double -> Double -> Double
	displacement_vat v a t = v*t - 1/2 * a * t^2 -- s=vt-(1/2)at^2
	displacement_uvt :: Double -> Double -> Double -> Double
	displacement_uvt u v t = ((u+v)/2)*t -- s=(u+v/2)t
	
	-- Fundamental mechanics as per Newton
	momentum :: Double -> Double -> Double
	momentum  m v = m * v -- p=mv mass times velocity
	forceNewtonian :: Double -> Double -> Double
	forceNewtonian m a= m * a -- NII F=ma
	force_pt :: Double -> Double -> Double -> Double
	force_pt p_1 p_2 t = (p_2 - p_1)/t -- force is change in momentum over time
	impulse :: Double -> Double -> Double
	impulse f t = f * t -- impulse is force times time
	keneticEnergy :: Double -> Double -> Double
	keneticEnergy m v = 1/2 * m * v^2 -- E_k=1/2mv^2

	-- circular motion
	angularVelocity_theatat :: Double -> Double -> Double
	angularVelocity_theatat theata t = theata / t -- angle over time | omega
	angularVelocity_vr :: Double -> Double -> Double
	angularVelocity_vr v r = v / r -- linear velocity over radius | omega
	centripitalAceleration_omegat :: Double -> Double -> Double
	centripitalAceleration_omegat omega t = omega^2 * t -- Change in angular velocity over time
	centripitalAceleration_vrt :: Double -> Double -> Double -> Double
	centripitalAceleration_vrt v r t = centripitalAceleration_omegat (angularVelocity_vr v r) t -- work out the angular aceleration when given a linear velocity and radius
	centripitalForce_momegar :: Double -> Double -> Double -> Double
	centripitalForce_momegar m omega r = m * omega^2 * r -- the force towards the center from the mass angular velocity and radius
	centripitalForce_mvr :: Double -> Double -> Double -> Double
	centripitalForce_mvr m v r = centripitalForce_momegar m (angularVelocity_vr v r) r -- get the force towards the center when given linar velovity
	
	-- Gravitation
	gravitationalConstant = 6.67e-11 -- G
	gravitationalForce  :: Double -> Double -> Double -> Double
	gravitationalForce m_1 m_2 r = (-gravitationalConstant * m_1 * m_2)/ r^2 -- Newtons universal law
	gravitationalFieldStrengh_Fm :: Double -> Double -> Double
	gravitationalFieldStrengh_Fm f m = f / m  -- g
	gravitationalFieldStrengh_Mr :: Double -> Double -> Double
	gravitationalFieldStrengh_Mr m r = gravitationalFieldStrengh_Fm (gravitationalForce m 1 r) m -- When given a radius rather than a force | g
	gravitationalPotential :: Double -> Double -> Double
	gravitationalPotential m r = (gravitationalConstant * m)/ r -- V
	workDone :: Double -> Double -> Double -> Double
	workDone m v_1 v_2 = m * (v_2 - v_1) -- w
	orbitalVelocity :: Double -> Double -> Double
	orbitalVelocity m r = sqrt ((gravitationalConstant * m)/r) -- velocity at witch an object must orbit due to gravity at a specified radius and mass | linear | v
	orbitalTimePeriod :: Double -> Double -> Double
	orbitalTimePeriod r v = 2 * pi * (r/v) -- Time taken to complete a complete orbit
