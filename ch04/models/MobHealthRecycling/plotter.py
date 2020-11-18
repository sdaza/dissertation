import numpy as np
import matplotlib.pyplot as plt

def create_plot(x, y):

    c = np.correlate(x, y)

    plt.plot(x, y, "o")
    #plt.title("Correlation " + c)
    plt.show()