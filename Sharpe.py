import yfinance as yf
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

r_f = 0.04  # riskitön korko

tickers = ["AAPL", "GOOGL", "MSFT"]
prices = yf.download(tickers, period="1y")["Close"]
returns = prices.pct_change().dropna()

market_prices = yf.download("^GSPC", period="1y")["Close"]
market_returns = market_prices.pct_change().dropna()

returns = returns.loc[market_returns.index]

returns_year = (1 + returns.mean())**252 - 1
market_return_year = (1 + market_returns.mean())**252 - 1

cov_matrix = returns.cov() * 252

n_portfolios = 1000
weights = np.random.dirichlet(np.ones(len(tickers)), n_portfolios)

portfolio_returns = []
portfolio_vols = []
sharpe_ratios = []
betas = []

returns_np = returns.values
market_np = market_returns.values.flatten()

for w in weights:
    # Tuotto ja volatiliteetti
    port_return = np.dot(w, returns_year)
    port_vol = np.sqrt(np.dot(w, np.dot(cov_matrix, w)))

    # Päivittäinen portfoliotuotto
    port_daily = returns_np @ w

    # Beta
    beta = np.cov(port_daily, market_np)[0, 1] / np.var(market_np)

    # Sharpe (KORJATTU)
    sharpe = (port_return - r_f) / port_vol

    # Tallenna
    portfolio_returns.append(port_return)
    portfolio_vols.append(port_vol)
    sharpe_ratios.append(sharpe)
    betas.append(beta)

portfolio_returns = np.array(portfolio_returns)
portfolio_vols = np.array(portfolio_vols)
sharpe_ratios = np.array(sharpe_ratios)
betas = np.array(betas)

# Maximize
max_idx = np.argmax(sharpe_ratios)

Optimal_port_return = portfolio_returns[max_idx]
Optimal_port_vol = portfolio_vols[max_idx]
Optimal_port_beta = betas[max_idx]

print("Max Sharpe Portfolio:")
print(f"Return = {Optimal_port_return:.2f}")
print(f"Volatility = {Optimal_port_vol:.2f}")
print(f"Beta = {Optimal_port_beta:.2f}")

# CML (Capital Market Line)
cml_x = np.linspace(0, max(portfolio_vols), 100)
cml_y = r_f + (Optimal_port_return - r_f) / Optimal_port_vol * cml_x

plt.figure(figsize=(8, 5))

plt.scatter(portfolio_vols, portfolio_returns,
            s=5, alpha=0.5, label="Random Portfolios")

plt.scatter(Optimal_port_vol, Optimal_port_return,
            color="red", s=50, label="Max Sharpe Portfolio")

plt.plot(cml_x, cml_y,
         linestyle="--", label="Capital Market Line")

plt.xlabel("Volatility (Std Dev)")
plt.ylabel("Annual Return")
plt.title("Efficient Frontier (Simulation) + CML")
plt.legend()
plt.show()