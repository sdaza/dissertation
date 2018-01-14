# import dependencies
import numpy as np 

# functions
def missing_data(df): 
    m = df.isnull().sum()/len(df)
    m = m[m>0.00]
    if ( len(m) > 0):
        return m.sort_values(ascending=False)
    else:
        print('No missing data!')
        
        
def transform_variables(data, variables, kind='log'):
    d = data.copy()
    for name in variables: 
        if kind=='log':
            values = [np.log(v) if v >0.0 else np.log(0.01) for v in d[name]]
            values = (values - np.mean(values)) # centering
        elif kind=='z':
            values = (d[name] - np.mean(d[name])) / np.std(d[name]) # z-score
        d[kind+'_'+name] = values
    return d