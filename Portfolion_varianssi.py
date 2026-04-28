
import yfinance as yf
import math 
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statistics


stocks = {
    "Apple": "AAPL",
    "Microsoft":"MSFT",
    "Amazon":"AMZN",
    "Nvidia":"NVDA",
    "Berkshire":"BRK-B",
    "Goldman Sachs":"GS",
    "United Health Group":"UNH",
    "Alphabet":"GOOG",
    "Hims":"HIMS",
    "JP Morgan":"JPM"
}

weights = {
    "Apple":0.10,
    "Microsoft":0.10,
    "Amazon":0.10,
    "Nvidia":0.10,
    "Berkshite":0.10,
    "Goldman Sachs":0.10,
    "United Health Group":0.10,
    "Alphabet":0.10,
    "Hims":0.10,
    "JP Morgan":0.10
}

price_data = pd.DataFrame()

for stock in stocks.values():
    historical_data = yf.download(stock, period ="200d",auto_adjust=True) #Huomioi vain kaupankäyntipäivät
    closing_price = pd.DataFrame(historical_data["Close"])
    daily_returns = closing_price.pct_change().dropna()
    combined = pd.concat([closing_price, daily_returns], axis=1) #Yhdistää riveittäin
    price_data = price_data.join(daily_returns, how="outer") #left, #right, #outer, #inner

#3. Määritä varianssi-kovarianssi -matriisi hintadatan perusteella

covariance_variance_matrix = price_data.cov()
df = pd.DataFrame(covariance_variance_matrix)


#Painomatriisin teko 
while sum(weights.values()) <= 1:
    weight_matrix = np.array([weights[name] for name in weights]).reshape(1, -1)
    weight_matrix_transpose = weight_matrix.T  # n x 1 
    break  

    
#5. Laske portfolion varianssi matriisien kertolaskulla käyttäen portfolioiden painoja sekä varianssi-kovarianssi -matriisia,(n,k) *(k,m) palauttaa (n x m)

Cov_var = pd.DataFrame.to_numpy(df, dtype=float, copy=False)
multiplication_1 = np.matmul(weight_matrix,Cov_var)
print(multiplication_1) 
Daily_var = np.matmul(multiplication_1,weight_matrix_transpose).item() 
Variance_year = Daily_var * 251
Volatility = np.sqrt(Variance_year)

print(f" Yearly variance of portfolion equals {round(Variance_year*100,2)} %")
print(f" Yearly volatility of portfolion equals {round(Volatility*100,2)} %")

#Draw a heatmap to show how much correlation different pairs have
# Punainen väri = iso korrelaatio
# Vihreä väri = pieni korrelaatio

#Standard deviations
#standard_deviation = np.var(Cov_var,axis=0)**(1/2) #Laskee sarakekohtaisen keskihajonnan
#print(standard_deviation)


#Korrelaatiot = kovarianssi/keskihajonta*keskihajonta
tickers = list(stocks.values())
names = list(stocks)
columns = ([stocks])
correlations = np.corrcoef(Cov_var)
correlations_df = pd.DataFrame(correlations, columns=tickers, index=tickers)

print(correlations_df) 
# Pitäisi vielä lisätä otsikot tähän dataframeen, jotta lukija tietää minkä osakkeiden välisistä korrelaatioista on kyse

price_data.to_csv("C:/Users/jerel/Downloads/price_data.csv", index=True)
correlations_df.to_csv("C:/Users/jerel/Downloads/correlations.csv", index=True)


fig, ax = plt.subplots()
im = ax.imshow(correlations)

# Show all ticks and label them with the respective list entries
ax.set_xticks(range(len(names)), labels=names,
              rotation=45, ha="right", rotation_mode="anchor",fontsize=8)
ax.set_yticks(range(len(names)), labels=names)

# Loop over data dimensions and create text annotations.
for i in range(len(names)):
    for j in range(len(names)):
        value = round(correlations[i,j],2)
        text = ax.text(j, i, value,
                       ha="center", va="center", color="black",fontsize=6)

ax.set_title("Correlations between chosen stocks")
plt.show()

pd.portfolio_volatility(price_data,weight_matrix_transpose)

#fine