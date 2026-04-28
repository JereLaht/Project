# -*- coding: utf-8 -*-
"""
Created on Fri Nov  7 21:27:02 2025

@author: jerel
"""
import numpy as np
import matplotlib.pyplot as plt
theta = 0.5
z = 1
alpha = 1
tax = np.linspace(0,1,num=100,endpoint=True,dtype=float)
G = tax*z*theta*(theta*(1-tax)/(alpha*(1-tax*theta)))**(theta)


plt.figure()
plt.plot(tax,G)
plt.title("Tax Return, theta=0.5, z=1, alpha=1")
plt.xlabel("Tax Rate")
plt.ylabel("Tax Return")
plt.show()

