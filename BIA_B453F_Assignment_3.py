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

# DBTITLE 1,Save the csv data as Delta format in Databricks Delta Data Lake
df.write.format("delta").mode("overwrite").option("overwriteSchema","True").saveAsTable('tinder_google_play_reviews_delta')

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
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.probability import FreqDist
import re
import string 

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
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator

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
#Creating the positive text variable
positive_text = " ".join(cat for cat in pd_df.content[pd_df["blob_sentiment_polarity"] >= 0.5])

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
positive_fdist = FreqDist()
for word in word_tokenize(positive_text):
    positive_fdist[word.lower()] += 1
    
positive_fdist.plot(30,title='Frequency for 30 most common tokens in word cloud with positive sentiment')

# COMMAND ----------

# DBTITLE 1,Word Cloud with negative sentiment
#Creating the negative text variable
negative_text = " ".join(cat for cat in pd_df.content[pd_df["blob_sentiment_polarity"] < 0.5])

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
negative_fdist = FreqDist()
for word in word_tokenize(negative_text):
    negative_fdist[word.lower()] += 1
    
negative_fdist.plot(30,title='Frequency for 30 most common tokens in word cloud with negative sentiment')

# COMMAND ----------

# DBTITLE 1,Count sentiment
sum_positive = sum(pd_df["blob_sentiment_polarity"] >= 0.5)
sum_negative = sum(pd_df["blob_sentiment_polarity"] < 0.5)

def sentiment_score(a, b):
    if (a>b):
        print("Positive 😊 ")
    else:
        print("Negative 😠 ")
       
sentiment_score(sum_positive, sum_negative)

print("Positive: ", sum_positive)
print("Negative: ", sum_negative)

# COMMAND ----------

# DBTITLE 1,Use Bash to download Spacy en_core_web_sm
# MAGIC %sh
# MAGIC python -m spacy download en_core_web_sm #Use bashscript to download Spacy en_core_web_sm

# COMMAND ----------

# DBTITLE 1,Text Network Analysis

import textnets as tn
import spacy
import en_core_web_sm

nlp = en_core_web_sm.load()

pd_fdist = spark.sql("SELECT content \
                        FROM transformed_tinder_google_play_reviews_delta \
                        WHERE content like '%fake%' \
                        ORDER BY thumbsUpCount ;").toPandas().head(30)

#tn.params["seed"] = 42

corpus = tn.Corpus.from_df(pd_fdist, doc_col="content")

t = tn.Textnet(corpus.tokenized(), min_docs=1)
#t = tn.Textnet(word for word in word_tokenize(text), min_docs=1)

t.plot(label_nodes=True, show_clusters=True)

# COMMAND ----------

# DBTITLE 1,Text similarity
import spacy

new_pd_fdist = spark.sql("SELECT content \
                        FROM transformed_tinder_google_play_reviews_delta \
                        WHERE content LIKE '%fake%' \
                        ORDER BY thumbsUpCount;").toPandas().head(30)

display(new_pd_fdist)

# COMMAND ----------

arr_negative_comment_similarity = []

for negative_comment_1 in new_pd_fdist.content:
    for negative_comment_2 in new_pd_fdist.content:
        negative_comment_similarity = nlp(negative_comment_1).similarity(nlp(negative_comment_2))
        print(negative_comment_similarity)
        arr_negative_comment_similarity.append(negative_comment_similarity)

# COMMAND ----------

np.mean(arr_negative_comment_similarity)

# COMMAND ----------

