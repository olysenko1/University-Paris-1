# -*- coding: utf-8 -*-
"""
Created on Mon Nov 17 01:35:22 2019

In this python file we're going to construct all the functions for 200 stocks in the portfolio, so covariance matrix, length and everything is designed to match 200 stocks
When we increase the number to study the assymptotic properties to 400 and 1600 we for simplicity created a separete python file, where we redisegned it to match 400 and 1600 respectively; So practically we just changed the dimension of covariance matrix, weigths of each stock to 0,25 and 0,0625 etc. Functions haven't been touched 

@author: Oleksandr
"""
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize
import scipy.stats as stats
from itertools import repeat

class EShortfall():
    
    def __init__(self, iterations, distribution = 'normal', esLevel = 5):
        """iterations: Number of iterations of sampling from distribution.
        distribution: Name of distribution to sample log-returns from.
        esLevel: Confidence level for expected shortfall and value at risk.
        """
        
        self.iterations = iterations
        self.distribution = distribution
        self.df = 200 #number of stocks in the portfolio, set to 400/1600 when we change it
        self.means = list(repeat(0, 200)) #mean is 0 for each stock; also get changed for 400/1600
        self.esLevel = esLevel
        self.initialCapital = 10**5
        flatted_input = list(repeat(1, 200))
        self.cov = np.diagflat(flatted_input)
        """
Covariance matrix - I'm just creating diagonal matrix of 200 elements. What if our returns are correlated;-> 
nonzero elements outside diagonal, it won't be correct anymore, need for different covariance matrix, but so many elements...

------------------------------------------------------------------------------
Moving towards exponential function, the way we defined it - all the stocks in our portfolio behave a bit differently, each following path with a small incremental 
When we increase the number of stocks to 400 and 1600 we didn't change the boundaries of the fluctuations - they still lie in between 0.03 and 0.07, but we reduce the incremental
"""    
    
    def exponential(self, vector):
       r=list(repeat(0, 200))
       n = np.arange(0.03, 0.07, 0.0002)
       for i in range(self.df):
           r[i] = np.exp(n[i] + 0.01 * vector[i])
           out = r
           return out
    """
We may say that they all follow the same exponential path
 def exponential(self, vector):
        r = list(repeat(0, 200))
        for i in range(self.df):
            r[i] = np.exp(0.03 + 0.01 * vector[i])
            out = r
        return out
----------------------------------------------------------------------------        
Now we define a simulation of returns functions for normal and students_t types of distributions with if and elif function. If we want to get simulated returns from the normal distribution then first algorithm 
""" 
    
    def simulation(self):
        self.normal_returns = np.random.multivariate_normal(self.means, self.cov, self.iterations)
        self.simulated_returns = []
        if self.distribution == 'normal':
            for i in range(self.iterations):
                self.simulated_returns.append(self.exponential(self.normal_returns[i]))
        elif self.distribution == 'students_t':
            for i in range(self.iterations):
                returns = self.exponential(self.t_transform(self.normal_returns[i], self.df))
                self.simulated_returns.append(returns)
                """
As for students_t distribution we just use vector transformation from norm to t. It's also probably gonna make the MC longer
"""
               
    
    def t_transform(self, vector, df):
        transformed_vector = []
        for x in vector:
            fx = stats.t.ppf(stats.norm.cdf(x), df)
            transformed_vector.append(fx)
        return transformed_vector
    """
Now we have all the returns, so we need to define ES (which relies on VaR, so that's how we also gonna get its estimation in each of the simulations) 
"""
    
    def es(self, weights):
        weights = np.divide(weights, np.sum(weights))#each stock is equally wieghted
        portfolio_losses = []
        for i in range(self.iterations):
            portfolio_return = np.dot(weights, self.simulated_returns[i])#The weight of stock*its return
            portfolio_loss = -self.initialCapital * (portfolio_return * np.exp(-0.03) - 1)# I put -0.03 but we might try different values
            portfolio_losses.append(portfolio_loss)
        portfolio_losses = np.array(portfolio_losses)
        var = np.percentile(portfolio_losses, 100 - self.esLevel)
        norm_ = round(self.iterations * 0.01 * self.esLevel)
        sum_ = np.sum(portfolio_losses[portfolio_losses > var])
        es = sum_/norm_
        return es, portfolio_losses
#to look for minimum es
    def objective_function(self, weights):
        es, _ = self.es(weights)
        return es
#These are required for defining optimal portfolio (by finding the optimal weights which minimize the ES )   
    def investment_constraint(self, weights):
        return np.sum(weights) - 1.0
    """
Boundmaker function is just to create a vector of n observations in which each observations has a value of (0,1). 
This is also necessary for future minimization function 
"""
    
    def boundmaker(self,n):
        bnd = [(0,1)] * n
        return bnd
##We use scipy.optimize to get the minimum ES (through choosing optimal weights for each of the stocks) conditionally on the way we designed our stocks' behaviour     
    def minimize_es(self, portfolio_losses):
        cons = {'type': 'eq', 'fun': self.investment_constraint}
        initial_weights = list(repeat(0.5, 200))
        bnds = self.boundmaker(200)
        opt = minimize(self.objective_function, initial_weights, bounds = bnds, constraints = cons)
        return opt.x
##Graphical representation of each MC simulations together with: mean return (remember we set it up to zero at the begging), VaR and ES  
    def plot_loss_distribution(self, portfolio_losses):
        n, bins, patches = plt.hist(x=portfolio_losses, bins='auto', range = (-1000, 1000))
        var = np.percentile(portfolio_losses, 100 - self.esLevel)
        plt.grid(axis='y', alpha=0.9)
        plt.xlabel('Loss')
        plt.ylabel('Frequency')
        plt.title('Histogram of Portfolio Losses')
        xlab = '95% Confidence VaR: ' + "{0:.2f}".format(var)
        plt.axvline(x=var, color = 'black', linestyle='--', label= xlab)
        plt.legend(loc='upper left')
        plt.show()


"""
MC Simulations 

First simulation is gonna provide us with VaR, ES, mean return for 200 stocks returns of which follow normal distribution at 10**6 interations.
The specification below is for 95% level of confidence; to get 90% and 99% reported in the studies we just set esLevel to 10 and 1 respectively
"""
def simulation_1():
    ES = EShortfall(iterations = 10**6, 
                           esLevel = 5, distribution = 'normal')
    portfolio_weights = list(repeat(0.5, 200))
    ES.simulation()
    es, portfolio_losses = ES.es(portfolio_weights)
    ES.plot_loss_distribution(portfolio_losses)
    print('Mean return: {}'.format(np.mean(portfolio_losses)))
    print('{}% confidence Expected Shortfall: {}'.format(100-ES.esLevel, es))

if __name__ == '__main__':
    simulation_1()


"""
Second simulation is gonna provide us with VaR, ES, mean return for 200 stocks returns of which follow students_t distribution at 10**6 interations.
The specification below is for 95% level of confidence; to get 90% and 99% reported in the studies we just set esLevel to 10 and 1 respectively
"""
def simulation_2():
    ES = EShortfall(iterations = 10**6, 
                       esLevel = 5, distribution = 'students_t')
    portfolio_weights = list(repeat(0.5, 200))
    ES.simulation()
    es, portfolio_losses = ES.es(portfolio_weights)
    ES.plot_loss_distribution(portfolio_losses)
    print('{}% confidence Expected Shortfall: {}'.format(100-ES.esLevel, es))   

if __name__ == '__main__':
    simulation_2()
"""
Third simulation is to get optimal portfolio through assigning the weights which minimize ES, the weights are reported in the variable optimal_weight 
"""
def simulation_3():
    ES = EShortfall(iterations = 10**5, esLevel = 5, distribution = 'normal')
    ES.simulation()
    initial_weights = list(repeat(0.5, 200))
    initial_es, portfolio_losses = ES.es(initial_weights)
    optimal_weights = ES.minimize_es(portfolio_losses)
    minimal_es, portfolio_losses = ES.es(optimal_weights)
    ES.plot_loss_distribution(portfolio_losses)
    print('Mean return: {}'.format(np.mean(portfolio_losses)))
    print('{}% confidence Expected Shortfall for optimal portfolio: {}'.format(100-ES.esLevel, minimal_es))
    optimal_weight = []
    optimal_weight.append(optimal_weights)

if __name__ == '__main__':
    simulation_3()

