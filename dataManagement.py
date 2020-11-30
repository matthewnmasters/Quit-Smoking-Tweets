#Data wrangling to make dataframes for analysis

import os
os.chdir('C:/Users/matth/QS2020/QuitSmokingTweets') #change as needed

#first make the files one single data frame
import pandas as pd


df_1 = pd.read_csv('0101_0317_2018.csv', encoding='utf-8-sig')
df_2 = pd.read_csv('0318_0515_2018.csv', encoding='utf-8-sig')
df_3 = pd.read_csv('0515_0623_2018.csv', encoding='utf-8-sig')
df_4 = pd.read_csv('0624_0922_2018.csv', encoding='utf-8-sig')
df_5 = pd.read_csv('0923_1124_2018.csv', encoding='utf-8-sig')
df_6 = pd.read_csv('1125_0209_2019.csv', encoding='utf-8-sig')
df_7 = pd.read_csv('0210_0510_2019.csv', encoding='utf-8-sig')
df_8 = pd.read_csv('0511_0804_2019.csv', encoding='utf-8-sig')
df_9 = pd.read_csv('0805_1001_2019.csv', encoding='utf-8-sig')
df_10 = pd.read_csv('1002_1210_2019.csv', encoding='utf-8-sig')
df_11 = pd.read_csv('1211_0221_2020.csv', encoding='utf-8-sig')
df_12 = pd.read_csv('0222_0523_2020.csv', encoding='utf-8-sig')
df_13 = pd.read_csv('0524_0823_2020.csv', encoding='utf-8-sig')
df_14 = pd.read_csv('0824_1101_2020.csv', encoding='utf-8-sig')
df_15 = pd.read_csv('1102_1125_2020.csv', encoding='utf-8-sig')


#merge them all together and drop any exact duplicates (there may be overlap around the end of each time period)
alldata = pd.concat([df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10, df_11, df_12, df_13, df_14, df_15]).drop_duplicates()

#remove duplicate posts from the same author, and also further remove tweets and authors UNAVAILABLE
nonDuplicate = alldata.drop_duplicates(subset=['AUTHOR', 'CONTENT'])



#write to csv with proper encoding
alldata.to_csv('mergedData.csv', index=False, encoding='utf-8-sig')
nonDuplicate.to_csv('noRedactedTweets.csv', index=False, encoding='utf-8-sig')

#2018 Tweets
nonDuplicate.dtypes
#2019 Tweets

#2020 Tweets