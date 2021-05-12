#%%
import numpy as np
import pandas as pd
from fbprophet import Prophet
from fbprophet.diagnostics import performance_metrics, cross_validation

#%%
#import data
full_df = pd.read_csv('CHW_Forecasting-main/DataCleaning/merged_data.csv')
#subset data
power_only_df = full_df[['DATE','Total Power']]
power_only_df = power_only_df.rename(columns={'DATE': 'ds', 'Total Power': 'y'})

#%%
#initialize Prophet model
m_power_only = Prophet(changepoint_prior_scale=0.01, daily_seasonality=True, weekly_seasonality=True, yearly_seasonality=True)
m_power_only.fit(power_only_df)
#%%
#Forecasting
future_power_only = m_power_only.make_future_dataframe(periods=1000, freq='h')
forecast_power_only = m_power_only.predict(future_power_only)

#%%
#plot predictions
fig_power_only_plot = m_power_only.plot(forecast_power_only)


# %%
#Adding Temp regressor
power_temp_df = full_df[['DATE', 'Total Power', 'HourlyDryBulbTemperature']]
power_temp_df = power_temp_df.rename(columns={'DATE': 'ds', 'Total Power': 'y'})
#%%
#new Prophet model
m_power_temp = Prophet(changepoint_prior_scale=0.01, daily_seasonality=True, weekly_seasonality=True, yearly_seasonality=True)
m_power_temp.add_regressor('HourlyDryBulbTemperature')
m_power_temp.fit(power_temp_df)
# %%
#Forecasting
future_power_temp = m_power_temp.make_future_dataframe(periods=2000, freq='h')
fake_future_temp_vals = full_df.loc[:,'HourlyDryBulbTemperature'].append(full_df.loc[:,'HourlyDryBulbTemperature'])
fake_future_temp_vals.reset_index(drop=True, inplace=True)
#%%
future_power_temp['HourlyDryBulbTemperature'] = fake_future_temp_vals[range(len(future_power_temp))]
forecast_power_temp = m_power_temp.predict(future_power_temp)
# %%
fig_power_temp_plot = m_power_temp.plot(forecast_power_temp)
# %%
#full_df[['HourlyDewPointTemperature', 'HourlyDryBulbTemperature', 'HourlyWetBulbTemperature']]
# %%
fig_power_temp_components = m_power_temp.plot_components(forecast_power_temp)

# %%
#import daily data
daily_df = pd.read_csv('daily_median_data.csv')
daily_df = daily_df.rename(columns={'DATE': 'ds', 'Median Daily Total Power': 'y'})
#%%
#new Prophet model
m_daily = Prophet(changepoint_prior_scale=0.01, weekly_seasonality=True, yearly_seasonality=True)
m_daily.add_regressor('Median Daily Temperature')
m_daily.fit(daily_df)
# %%
#Forecasting
future_daily = m_daily.make_future_dataframe(periods=60)
fake_daily_temp_vals = daily_df.loc[:,'Median Daily Temperature'].append(daily_df.loc[:,'Median Daily Temperature'])
fake_daily_temp_vals.reset_index(drop=True, inplace=True)
#%%
future_daily['Median Daily Temperature'] = fake_daily_temp_vals[range(len(future_daily))]
forecast_daily = m_daily.predict(future_daily)
# %%
fig_daily_plot = m_daily.plot(forecast_daily)
# %%
fig_daily_components = m_daily.plot_components(forecast_daily)
# %%
df_daily_cv = cross_validation(m_daily, initial='90 days', period='7 days', horizon='45 days')

# %%
df_daily_p = performance_metrics(df_daily_cv)
# %%
