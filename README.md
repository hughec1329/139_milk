# milk pricing project - present 5/29 in williams OH

## cross hedge
- form cross hedge with traded commodoties to most closely reflect farmgate price milk.

- traded commodoties for inclusion - all from http://future.aae.wisc.edu/tab/prices.html#3, secion CME Butter and Cheese Spot Prices (Daily, Weekly, and Monthly)
* CME block cheddar
* CME butter
* CME (?) NFDM - grade A and extra Grade.

### techniques
- look at cors,
- regression - farmgate dependent.
* adjust for auto cor??
* ARIMA?? - take to tsai?

## optimal hedging ratio
- use cross hedge from part I, calculate optimal hedging ratio
- HR = d cash / d futures
- OLS - find dCP = B0 + B1 dFP

# Look at implied volatility in milk options
- use short dated option - source??
- compare with price
- look at seasonality - cf corn - affected by weather at planting ( may) and tasseling (??) - spikes.


