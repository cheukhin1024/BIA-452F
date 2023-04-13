# Databricks notebook source
# DBTITLE 1,Install libaries
import numpy as np
import pandas as pd

from pyspark import SparkFiles
from pyspark import SparkContext
from pyspark.sql import functions
import pyspark.sql.functions #import avg, col, udf
from pyspark.sql import SQLContext
from pyspark.sql import DataFrame
from pyspark.sql.types import *

import seaborn as sns

# COMMAND ----------

# DBTITLE 1,Use Apache Spark to read the csv data
path = "dbfs:/user/hive/warehouse/tinder_google_play_reviews"
df = spark.read.format("delta").options(header="true").load(path)
display(df)

# COMMAND ----------

#df.write.format("delta").mode("overwrite").option("overwriteSchema","True").saveAsTable('tinder_google_play_reviews_delta')

# COMMAND ----------

# DBTITLE 1,#Drop columns from data frame
df = spark.sql("SELECT content, \
                       score, \
                       thumbsUpCount \
                FROM tinder_google_play_reviews_delta \
                ORDER BY at DESC")
display(df)                 

# COMMAND ----------

# DBTITLE 1,Show data types
df.dtypes

# COMMAND ----------

# DBTITLE 1,Missing data
display(df) 

# COMMAND ----------

# DBTITLE 1,Drop rows with null values & convert Spark dataframe into Pandas dataframe
#Drop rows with null values & Covert Spark dataframe into Pandas dataframe
pd_df = df.na.drop("any").toPandas()
display(pd_df)

# COMMAND ----------

# DBTITLE 1,Histograms of 'score'
sns.countplot(pd_df.score)

# COMMAND ----------

# DBTITLE 1,Find the top contents with most 'thumbsUpCount'
display(spark.sql("SELECT content, thumbsUpCount FROM tinder_google_play_reviews_delta ORDER BY thumbsUpCount DESC"))

# COMMAND ----------

# DBTITLE 1,Covert text string into numerical sentiment score
from textblob import TextBlob

arr_blob_sentiment_polarity = []

for text in pd_df["content"]:
    blob = TextBlob(text)
    blob_sentiment_polarity = blob.sentiment.polarity
    arr_blob_sentiment_polarity.append(blob_sentiment_polarity)

pd_df.insert(3, "blob_sentiment_polarity", arr_blob_sentiment_polarity)
#If you run this script before, there will be an error: ValueError: cannot insert blob_sentiment_polarity, already exists.  You can ignore the error and still run the following remaining scripts succesfully.
display(pd_df)

# COMMAND ----------

pd_df_spark = spark.createDataFrame(pd_df)
pd_df_spark.write.format("delta").mode("overwrite").option("overwriteSchema","True").saveAsTable('transformed_tinder_google_play_reviews_delta')

# COMMAND ----------

# DBTITLE 1,Correlation Matrix 
import matplotlib.pyplot as plt

plt.figure(figsize=(30,35))
sns.heatmap(pd_df.corr(),  cmap='coolwarm', vmax=.3, center=0,square=True, linewidths=.5,annot=True)
plt.show()

# COMMAND ----------

# DBTITLE 1,Removal of Stop Words
import nltk
nltk.download('punkt')
nltk.download('stopwords')
from nltk.tokenize import word_tokenize
from nltk.probability import FreqDist
import re

stemmer=nltk.SnowballStemmer('english')
stopword=set(stopwords.words('english'))

def clean(text):
    text=str(text).lower()
    text = re.sub('\[.*?\]', '', text)
    text = re.sub('https?://\S+|www\.\S+', '', text)
    text = re.sub('<.*?>+', '', text)
    text = re.sub('[%s]' % re.escape(string.punctuation), '', text)
    text = re.sub('\n', '', text)
    text = re.sub('\w*\d\w*', '', text)
    text = [word for word in text.split(' ') if word not in stopword]
    text = " ".join(text)
    text = [stemmer.stem(word) for word in text.split(' ')]
    text = " ".join(text)
    return text

pd_df['content']=pd_df['content'].apply(clean)

# COMMAND ----------

# DBTITLE 1,Overall Word Cloud
#Creating the text variable
text = " ".join(cat for cat in pd_df.content)

#Generate word cloud
word_cloud = WordCloud(
        width=3000,
        height=2000,
        random_state=1,
        background_color="salmon",
        colormap="Pastel1",
        collocations=False,
        stopwords=STOPWORDS,
        ).generate(text)

#Display the generated Word Cloud
plt.imshow(word_cloud)
plt.axis("off")
plt.show()

# COMMAND ----------

# DBTITLE 1,Frequency for 30 most common tokens in overall word cloud
fdist = FreqDist()
for word in word_tokenize(text):
    fdist[word.lower()] += 1
    
fdist.plot(30,title='Frequency for 30 most common tokens in overall word cloud')

# COMMAND ----------

# DBTITLE 1,Word Cloud with positive sentiment
#Creating the text variable
positive_text = " ".join(cat for cat in pd_df.content[pd_df["blob_sentiment_polarity"] > 0])

#Generate word cloud
word_cloud = WordCloud(
        width=3000,
        height=2000,
        random_state=1,
        background_color="salmon",
        colormap="Pastel1",
        collocations=False,
        stopwords=STOPWORDS,
        ).generate(positive_text)

#Display the generated Word Cloud
plt.imshow(word_cloud)
plt.axis("off")
plt.show()

# COMMAND ----------

# DBTITLE 1,Frequency for 30 most common tokens in word cloud with positive sentiment
fdist = FreqDist()
for word in word_tokenize(positive_text):
    fdist[word.lower()] += 1
    
fdist.plot(30,title='Frequency for 30 most common tokens in word cloud with positive sentiment')

# COMMAND ----------

# DBTITLE 1,Word Cloud with neutral sentiment
#Creating the text variable
neutral_text = " ".join(cat for cat in pd_df.content[pd_df["blob_sentiment_polarity"] == 0])

#Generate word cloud
word_cloud = WordCloud(
        width=3000,
        height=2000,
        random_state=1,
        background_color="salmon",
        colormap="Pastel1",
        collocations=False,
        stopwords=STOPWORDS,
        ).generate(neutral_text)

#Display the generated Word Cloud
plt.imshow(word_cloud)
plt.axis("off")
plt.show()

# COMMAND ----------

# DBTITLE 1,Frequency for 30 most common tokens in word cloud with neutral sentiment
fdist = FreqDist()
for word in word_tokenize(neutral_text):
    fdist[word.lower()] += 1
    
fdist.plot(30,title='Frequency for 30 most common tokens in word cloud with neutral sentiment')

# COMMAND ----------

# DBTITLE 1,Word Cloud with negative sentiment
#Creating the text variable
negative_text = " ".join(cat for cat in pd_df.content[pd_df["blob_sentiment_polarity"] < 0])

#Generate word cloud
word_cloud = WordCloud(
        width=3000,
        height=2000,
        random_state=1,
        background_color="salmon",
        colormap="Pastel1",
        collocations=False,
        stopwords=STOPWORDS,
        ).generate(positive_text)

#Display the generated Word Cloud
plt.imshow(word_cloud)
plt.axis("off")
plt.show()

# COMMAND ----------

# DBTITLE 1,Frequency for 30 most common tokens in word cloud with negative sentiment
fdist = FreqDist()
for word in word_tokenize(negative_text):
    fdist[word.lower()] += 1
    
fdist.plot(30,title='Frequency for 30 most common tokens in word cloud with negative sentiment')

# COMMAND ----------

# DBTITLE 1,Clustering
"""
#tfidf vector initililization
from sklearn.feature_extraction.text import TfidfVectorizer

tfidf_vect = TfidfVectorizer()
tfidf = tfidf_vect.fit_transform(text.values)
tfidf.shape
"""