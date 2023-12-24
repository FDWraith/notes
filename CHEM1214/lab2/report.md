Kevin Zhang
Lab Report 2

#### Introduction

The goal of this lab is to measure the heat of reactions. In particular, we are measuring the temperature changes of two reactions in order to determine the heat of reaction for a third composite reaction (which can be expressed as a combination of the two other reactions). 

#### Chemical Responsibility

We are dealing with acids and bases here. In particular, HCl is toxic and corrosive, and NaOH is toxic, corrosive, and easily absorbed by skin. Furthermore, Mg(s) is a flammable metal. If any of these substances reach eyes or skin, they should be rinsed immediately with cold water.

#### Report Sheet 

Part 1

| Measurement                       | Value                |
| --------------------------------- | -------------------- |
| HCl Initial Temp ($^{\circ}$C)    | 15.3                 |
| NaOH Initial Temp ($^{\circ}$C)   | 15.2                 |
| Final Temp ($^{\circ}$C)          | 28.8                 |
| Temperature Change ($^{\circ}$C)  | 13.55                |
| $\Delta H$ (J)                    | 8.50 $\times$ $10^3$ |
| $\Delta H$ (kJ)                   | 8.50                 |
| Moles of Water (mol)              | 0.15                 |
| $\Delta H$ per mol water (kJ/mol) | 56.7                 |

Part 2

| Measurement                      | Value                 |
| -------------------------------- | --------------------- |
| Mass of Mg (g)                   | 0.292                 |
| Moles of Mg (mol)                | 0.0120                |
| Initial Temp ($^{\circ}$C)       | 15.9                  |
| Final Temp ($^{\circ}$C)         | 23.0                  |
| Temperature Change ($^{\circ}$C) | 7.1                   |
| $\Delta H$ (J)                   | 4.456 $\times$ $10^3$ |
| $\Delta H$ (kJ)                  | 4.45                  |
| $\Delta H$ per mol Mg (kJ/mol)   | 371.33                |

Part 3
$$
\begin{align}
&(1) &H^+ + OH^- \rightarrow H_2O &&\Delta H = -56.7 \text{kJ/mol} \\
&(1)^* &H_2O \rightarrow H^+ + OH^- &&\Delta H = 56.7 \text{kJ/mol} \\
&(2) &Mg(s) + 2H^+ \rightarrow Mg^{2+} + H_2 &&\Delta H = -371.33 \text{kJ/mol} \\
&(3) &Mg(s) + 2H_2O \rightarrow Mg^{2+} + 2OH^-  &&\Delta H = \space ?
\end{align}
$$
We can express $\Delta H_{(3)}$ in terms of its composites:
$$
\Delta H_{(3)} = \Delta H_{(2)} + 2\Delta H_{(1)^*} \\
\Delta H_{(3)} = -371.33 kJ/mol \space + \space 2(56.7 kJ/mol) \\
\Delta H_{(3)} = -257.93 kJ/mol
$$


#### Sample Calculations

$$
\Delta H = mc\Delta T = (150g)(4.184 J/g^{\circ}C)(13.55^{\circ}C) = 8500 J \\
\text{mol of water} = \frac{1 \text{ mol of water }}{1 \text{ mol of HCl}} \times\frac{2.0 \text{ mol of HCl}}{1 L}\times \frac{1 L}{1000 mL} \times 75 mL = 0.15 \text{ mol} \\
\Delta H \text{ per mol of water} = \frac{\Delta H}{\text{mol of water}} = \frac{8.50kJ}{0.15 mol} = 56.7 kJ/mol
$$

#### Discussion of Results

We can determine the expected heat of reaction using enthalpy of formation. As such:
$$
\Delta H^{\circ}_{H_2O} = -286kJ/mol \\
\Delta H^{\circ}_{Mg(s)} = 0kJ/mol \\
\Delta H^{\circ}_{Mg^{2+}} = -462kJ/mol \\
\Delta H^{\circ}_{OH^-} = -230kJ/mol \\
\\
\Delta H^{\circ}_{rxn} = \sum_{products} \Delta H^{\circ} - \sum_{reactants} \Delta H^{\circ} \\
\Delta H^{\circ}_{rxn} = (-462 + 2(-230)) - (0 + (2(-286)) = -350 kJ/mol
$$
With our lab results of -257.93kJ/mol and an expected heat of reaction of -350kJ/mol, we have a margin of error of 26%. This is rather surprising. We aren't entirely sure where the error would be coming from, but some issues might come from small measurement errors from pouring HCl and NaOH or diluting HCl. 

#### Post-Lab Questions

1. True or False: Heat always flow from the hotter system to the cooler system.
   True

2. Were the reactions you performed exothermic or endothermic? Why?
   Exothermic. The heat left the system and warmed up the water surrounding it.

3. Define specific heat.
   The specific heat represents that amount of energy needed to raise one gram of a material by one degree C. The lower the specific heat, the less energy needed to warm it.

4. True or False: Enthalpy is defined as the change in heat at constant pressure
   True

5. True or False: The amount of heat absorbed or evolved is proportional to the masses of the reacting materials
   True

6. Consider the reaction $HCl + NaOH \rightarrow NaCl + H_2O$
   Given following information:
   Initial Temp for HCl ($^{\circ}$C): 22
   Initial Temp for NaOH ($^{\circ}$C): 22
   Final Temp ($^{\circ}$C): 26.1
   Temperature Change: 4.1

   1. Calculate the amount of heat evolved when 15 mL of 1.0 M HCl was mixed with 35 mL of 1.0 M NaOH
      $$
      \Delta H = mc\Delta T = (50 g)(4.1^{\circ}C)(4.184J/g^{\circ}C) = 857.72 J = 0.858 kJ
      $$
      

   2. Calculate $\Delta H^{\circ}$ per mole of water formed
      $$
      \text{mol of water} = (15 mL)(\frac{1L}{1000mL})(\frac{1 mol}{1L}) = 0.015 mol \\
      \frac{\Delta H}{\text{mol of water}} =  \frac{0.858 kJ}{0.015 mol} = 57.2 kJ/mol
      $$
      

#### Conclusion

In conclusion, you can find the heat of a reaction by finding the heats of reaction for composite reactions, and then using Hess's Law to find the heat of the reaction you are looking for.

