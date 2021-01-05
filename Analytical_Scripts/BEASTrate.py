#!/usr/bin/env python
# coding: utf-8

# In[ ]:

## input from command: rate_combined.log BSSVS.txt (from SPREAD3) output.csv
## output output.csv summarize the rate and BSSVS
## rate summary


# In[32]:


import pandas as pd
import numpy as np
import sys


# In[33]:


def calc_min_interval(x, alpha):
    """Internal method to determine the minimum interval of a given width
    Assumes that x is sorted numpy array.
    """

    n = len(x)
    cred_mass = 1.0-alpha

    interval_idx_inc = int(np.floor(cred_mass*n))
    n_intervals = n - interval_idx_inc
    interval_width = x[interval_idx_inc:] - x[:n_intervals]

    if len(interval_width) == 0:
        raise ValueError('Too few elements for interval calculation')

    min_idx = np.argmin(interval_width)
    hdi_min = x[min_idx]
    hdi_max = x[min_idx+interval_idx_inc]
    return hdi_min, hdi_max


# In[34]:


def hpd(x, alpha=0.05):
    """Calculate highest posterior density (HPD) of array for given alpha.
    The HPD is the minimum width Bayesian credible interval (BCI).
    :Arguments:
        x : Numpy array
        An array containing MCMC samples
        alpha : float
        Desired probability of type I error (defaults to 0.05)
    """

    # Make a copy of trace
    x = x.copy()
    # For multivariate node
    if x.ndim > 1:
        # Transpose first, then sort
        tx = np.transpose(x, list(range(x.ndim))[1:]+[0])
        dims = np.shape(tx)
        # Container list for intervals
        intervals = np.resize(0.0, dims[:-1]+(2,))

        for index in make_indices(dims[:-1]):
            try:
                index = tuple(index)
            except TypeError:
                pass

            # Sort trace
            sx = np.sort(tx[index])
            # Append to list
            intervals[index] = calc_min_interval(sx, alpha)
        # Transpose back before returning
        return np.array(intervals)
    else:
        # Sort univariate node
        sx = np.sort(x)
        return np.array(calc_min_interval(sx, alpha))


# In[37]:


def main():
    rates = pd.read_table(sys.argv[1])
    rows = []
    for i in rates:
        column = rates[i].replace(0,np.NaN)
        sorted = column.sort_values()
        NoNan = sorted.dropna()
        interval = hpd(NoNan)
        rows.append([i,sorted.mean(),sorted.median(),interval[0],interval[1]])
        
    df = pd.DataFrame(rows, columns=["Pathway", "mean.rate","median.rate","95_HPD.low","95_HPD.high"])
    df=df[df['Pathway'].str.contains('rates')]
    df[['code','rates',"FROM","TO"]] = df['Pathway'].str.split('.',expand=True)
    ## read the Bayesfactor table and combine together
    BSSVS = pd.read_table(sys.argv[2])
    df2 = pd.merge(df, BSSVS, left_on=['FROM','TO'], right_on = ['FROM','TO'])
    df2 = df2[['FROM', 'TO', 'BAYES_FACTOR',"POSTERIOR PROBABILITY","mean.rate",'median.rate',"95_HPD.low","95_HPD.high"]]
    df2=df2.sort_values(by=['BAYES_FACTOR'],ascending=False)      
    df2.to_csv(sys.argv[3],index = False)
main()    
    
    

