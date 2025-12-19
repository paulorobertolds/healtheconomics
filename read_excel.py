import pandas as pd
try:
    df = pd.read_excel('parameters.xlsx')
    print(df.to_string())
except Exception as e:
    print(e)
