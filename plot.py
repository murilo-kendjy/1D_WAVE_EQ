import matplotlib.pyplot as plt
import numpy as np
import matplotlib.animation as animation
from matplotlib import rcParams
import sys
import re

rcParams['animation.convert_path'] = r'/usr/bin/convert'

u=[]
ux=[]
x=[]
t=[]

# Data file for the matrix u(i,j) 
df= sys.argv[1]
var1= re.findall('\|(\d{1,4})', df)

# Variables necessary to plot 
Nx=  int(var1[0])
Nt=  int(var1[1])
Nx1= int(var1[2])
Nx2= int(var1[3])
NxL= int(var1[4])

# Temporal analises, store the columns
data=(open(df, "r"))
for i in data.readlines():
    line= i.split()
    u.append(line)

# Data file for the vecs x and t
vecs= sys.argv[2]

v = (open(vecs, "r"))
for i in v.readlines():
    line= i.split()
    if (len(line) != 1):
        x.append(line[0])
        t.append(line[1])
    elif (len(line) == 1 & Nx < Nt):
        x.append(line[0])
    elif (len(line) == 1 & Nx > Nt):
        t.append(line[0])

# Convert to numpy arrays
ux = np.array([ x for item in u for x in item]).astype(float)
x= np.array(x).astype(float)
t= np.array(t).astype(float)

fig, ax = plt.subplots(figsize=(12, 6))
ax.set_xlabel('x')
ax.set_ylabel('u(x,t)')
ax.set_xlim(min(x),max(x))
ax.set_ylim(min(ux)*2.5,max(ux)*2.5)
ax.set_title('Equação de onda na corda')

i=0; k=1; j=0

wave1, = ax.plot(0,0, '-b', lw=.9, label=f'Rope 1, μ1')
if (Nx1 != Nx):
    wave2, = ax.plot(0,0, '-r', lw=.9, label=f'Rope 2, μ3')
if (NxL > 0):
    wave3, = ax.plot(0,0, '-g', lw=.9, label=f'Joint, μ2')

plt.legend(loc='best')

def animation_frame(i):  
    wave1.set_xdata(x[:Nx1])
    wave1.set_ydata(ux[i:i+Nx1])
    if (Nx1 != Nx):
        wave2.set_xdata(x[Nx1+NxL-1:Nx])
        wave2.set_ydata(ux[i+Nx1+NxL-1:i+Nx])
    if (NxL > 0):
        wave3.set_xdata(x[Nx1-1:Nx1+NxL])
        wave3.set_ydata(ux[i+Nx1-1:i+Nx1+NxL])
    return wave1,

myAnimation= animation.FuncAnimation(fig, func=animation_frame, frames= np.arange(0,Nt*Nx, 4*Nx), interval=40, repeat=False)
plt.show()

# myAnimation.save('Eq-onda-3-cordas.gif', writer='imagemagick', fps=30)
