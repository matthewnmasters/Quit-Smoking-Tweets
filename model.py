#bring in all the data and create common dictionary, etc

import os
import nltk
os.chdir('C:/Users/matth/QS2020/QuitSmokingTweets')


import pandas as pd

#bring in the single file. Dates with Pandas and in Python in general are even more confusing than R
allYears = pd.read_csv('noRedactedTweets.csv')

data2018 = allYears[allYears["PUBLISH_DATE"].str.contains("2018") == True]
data2019 = allYears[allYears["PUBLISH_DATE"].str.contains("2019") == True]
data2020 = allYears[allYears["PUBLISH_DATE"].str.contains("2020") == True]

#make tweets all lowercase here, rather than later because we can filter it easier as a data frame
data2018["CONTENT"] = data2018["CONTENT"].str.lower()
data2019["CONTENT"] = data2019["CONTENT"].str.lower()
data2020["CONTENT"] = data2020["CONTENT"].str.lower()

#remove all URLs from tweets to facilitate removing of spam/bot posts of almost identical tweets
#could this result in blank tweets that cause a problem?
import re
data2018['CONTENT'] = data2018['CONTENT'].apply(lambda x: re.sub(r'http\S+', '', x))
data2019['CONTENT'] = data2019['CONTENT'].apply(lambda x: re.sub(r'http\S+', '', x))
data2020['CONTENT'] = data2020['CONTENT'].apply(lambda x: re.sub(r'http\S+', '', x))

#remove tweets that contain "weed"
data2018 = data2018[data2018["CONTENT"].str.contains("weed") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("weed") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("weed") == False]

#remove tweets that contain "blunt"
data2018 = data2018[data2018["CONTENT"].str.contains("blunt") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("blunt") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("blunt") == False]

#remove tweets that contain "crack"
data2018 = data2018[data2018["CONTENT"].str.contains("crack") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("crack") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("crack") == False]

#remove tweets that contain "roach"
data2018 = data2018[data2018["CONTENT"].str.contains("roach") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("roach") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("roach") == False]

#remove tweets that contain "baked"
data2018 = data2018[data2018["CONTENT"].str.contains("baked") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("baked") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("baked") == False]

#remove tweets that contain "dabs"
data2018 = data2018[data2018["CONTENT"].str.contains("dabs") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("dabs") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("dabs") == False]

#remove tweets that contain "marijuana"
data2018 = data2018[data2018["CONTENT"].str.contains("marijuana") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("marijuana") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("marijuana") == False]

#remove tweets that contain "bong"
data2018 = data2018[data2018["CONTENT"].str.contains("bong") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("bong") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("bong") == False]

#remove tweets that contain "mary jane"
data2018 = data2018[data2018["CONTENT"].str.contains("mary jane") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("mary jane") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("mary jane") == False]

#remove tweets that contain "maryjane"
data2018 = data2018[data2018["CONTENT"].str.contains("maryjane") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("maryjane") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("maryjane") == False]

#remove tweets that contain "pot"
data2018 = data2018[data2018["CONTENT"].str.contains("pot") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("pot") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("pot") == False]

######################################################################################################
#Create dataframes for each "type" of tweet

#only original posts:
posts2018 = data2018[data2018.POST_TYPE.isnull()]
posts2019 = data2019[data2019.POST_TYPE.isnull()]
posts2020 = data2020[data2020.POST_TYPE.isnull()]

#only retweets:
retweets2018 = data2018[data2018.POST_TYPE.eq("RETWEET")]
retweets2019 = data2019[data2019.POST_TYPE.eq("RETWEET")]
retweets2020 = data2020[data2020.POST_TYPE.eq("RETWEET")]


#only quote tweets:
quoteTweets2018 = data2018[data2018.POST_TYPE.eq("QUOTE_TWEET")]
quoteTweets2019 = data2019[data2019.POST_TYPE.eq("QUOTE_TWEET")]
quoteTweets2020 = data2020[data2020.POST_TYPE.eq("QUOTE_TWEET")]
  
#quote tweets AND retweets:
nonOriginaltweets2018 = data2018[data2018.POST_TYPE.notnull()]
nonOriginaltweets2019 = data2019[data2019.POST_TYPE.notnull()]
nonOriginaltweets2020 = data2020[data2020.POST_TYPE.notnull()]


#################################################################################################################
#Now we are ready to just look at processing the personal tweets, not retweets or quotes.
#################################################################################################################

#remove duplicate posts from different authors. We already removed dupes from the same authors, but theres lots of bot spam still
posts2018 = posts2018.drop_duplicates(subset=['CONTENT'])
posts2019 = posts2019.drop_duplicates(subset=['CONTENT'])
posts2020 = posts2020.drop_duplicates(subset=['CONTENT'])

#132558-115521 , 109688-103166 , 108237-103979
#Create a list of tweets instead of a data frame

justTweets2018 = list(posts2018['CONTENT'])
justTweets2019 = list(posts2019['CONTENT'])
justTweets2020 = list(posts2020['CONTENT'])

#POS tag then lemmatize. This is slow but I've had trouble using pos_tag_sents and converting those tupples to wordnet tags.
#It's really weird how the nltk tags aren't compatible. Annoying, but this doesnt take TOO long, at least. Will need to spend
#some time optimizing this if we ever do it with a larger data set (in the millions)
from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet
from nltk.tokenize import RegexpTokenizer

lemmatizer = WordNetLemmatizer()
tokenizer = RegexpTokenizer('\s+', gaps=True)

# function to convert nltk tag to wordnet tag
def nltk_tag_to_wordnet_tag(nltk_tag):
    if nltk_tag.startswith('J'):
        return wordnet.ADJ
    elif nltk_tag.startswith('V'):
        return wordnet.VERB
    elif nltk_tag.startswith('N'):
        return wordnet.NOUN
    elif nltk_tag.startswith('R'):
        return wordnet.ADV
    else:          
        return None

def lemmatize_sentence(sentence):
    #tokenize the sentence and find the POS tag for each token
    nltk_tagged = nltk.pos_tag(tokenizer.tokenize(sentence))  
    #tuple of (token, wordnet_tag)
    wordnet_tagged = map(lambda x: (x[0], nltk_tag_to_wordnet_tag(x[1])), nltk_tagged)
    lemmatized_sentence = []
    for word, tag in wordnet_tagged:
        if tag is None:
            #if there is no available tag, append the token as is
            lemmatized_sentence.append(word)
        else:        
            #else use the tag to lemmatize the token
            lemmatized_sentence.append(lemmatizer.lemmatize(word, tag))
    return " ".join(lemmatized_sentence)

justTweets2018 = [lemmatize_sentence(doc) for doc in justTweets2018]
justTweets2019 = [lemmatize_sentence(doc) for doc in justTweets2019]
justTweets2020 = [lemmatize_sentence(doc) for doc in justTweets2020]

#We need to lemmatize again. Had trouble making the code return a list. Still learning...

for idx in range(len(justTweets2018)):
    justTweets2018[idx] = tokenizer.tokenize(justTweets2018[idx])  # Split into words.
    
for idx in range(len(justTweets2019)):
    justTweets2019[idx] = tokenizer.tokenize(justTweets2019[idx])  # Split into words.

for idx in range(len(justTweets2020)):
    justTweets2020[idx] = tokenizer.tokenize(justTweets2020[idx])  # Split into words.


# Remove numbers, but not words that contain numbers.
justTweets2018 = [[token for token in doc if not token.isnumeric()] for doc in justTweets2018]
justTweets2019 = [[token for token in doc if not token.isnumeric()] for doc in justTweets2019]
justTweets2020 = [[token for token in doc if not token.isnumeric()] for doc in justTweets2020]

# Remove words that are only one character. #changed to more than two characters, do we want to consider stop words?
justTweets2018 = [[token for token in doc if len(token) > 2] for doc in justTweets2018]
justTweets2019 = [[token for token in doc if len(token) > 2] for doc in justTweets2019]
justTweets2020 = [[token for token in doc if len(token) > 2] for doc in justTweets2020]


# Remove stop words
from nltk.corpus import stopwords
#nltk.download('stopwords')
justTweets2018 = [[token for token in doc if token not in stopwords.words('english')] for doc in justTweets2018]
justTweets2019 = [[token for token in doc if token not in stopwords.words('english')] for doc in justTweets2019]
justTweets2020 = [[token for token in doc if token not in stopwords.words('english')] for doc in justTweets2020]

#################################################################################################################

# Compute bigrams. Currently this adds the bigram to the end, but that introduces bias. maybe just keep the bigram version
from gensim.models import Phrases

# Add bigrams to docs (only ones that appear 20 times or more). May want to repeat it for trigrams
bigram2018 = Phrases(justTweets2018, min_count=20)
bigram2019 = Phrases(justTweets2019, min_count=20)
bigram2020 = Phrases(justTweets2020, min_count=20)

justTweets2018= list(bigram2018[justTweets2018])
justTweets2019= list(bigram2019[justTweets2019])
justTweets2020= list(bigram2020[justTweets2020])

justTweetsAll= justTweets2018 + justTweets2019 + justTweets2020


# Remove rare and common tokens.
from gensim.corpora import Dictionary

# Create a dictionary representation of all the documents from all the years.
dictionaryAll = Dictionary(justTweetsAll)

# Filter out words that occur less than 20 documents, or more than 50% of the documents
# The last part was removed since I want them in there because theyre search terms, but it was no_above=0.5
dictionaryAll.filter_extremes(no_below=20)

#################################################################################################################

# Bag-of-words representation of the documents.
corpusAll = [dictionaryAll.doc2bow(doc) for doc in justTweetsAll]
corpus2018 = [dictionaryAll.doc2bow(doc) for doc in justTweets2018]
corpus2019 = [dictionaryAll.doc2bow(doc) for doc in justTweets2019]
corpus2020 = [dictionaryAll.doc2bow(doc) for doc in justTweets2020]

# Let's see how many tokens and documents we have to train on
print('Number of unique tokens: %d' % len(dictionaryAll))
print('Number of documents: %d' % len(corpusAll))

##################################################################################################################

#monte carlo simulation to identify number of topics

from gensim.models import LdaModel
from gensim.models import CoherenceModel


# Set training parameters.
chunksize = 115521
passes = 10
iterations = 200
eval_every = None  # evaluate perplexity

# Make a index to word dictionary.
temp = dictionaryAll[0]  # This is only to "load" the dictionary.
id2word = dictionaryAll.id2token

#montecarlo showed 8,11,13 topics as being a decent spot for c_v coherence
for i in range(2,51,1):
    model = LdaModel(
        corpus=corpus2018,
        id2word=id2word,
        chunksize=chunksize,
        alpha='auto',
        eta='auto',
        iterations=iterations,
        num_topics=i,
        passes=passes,
        eval_every=eval_every
        )

# Calculate coherence
    coherence_model_lda = CoherenceModel(model=model, texts=justTweets2018, dictionary=dictionaryAll, coherence='c_v')
    coherence_lda = coherence_model_lda.get_coherence()
    print('Num Topics: ', i, 'coherence is: ', coherence_lda)

######################################################################################################################

# Train LDA model with more passes, iterations
from gensim.models import LdaModel
from gensim.models import CoherenceModel

# Set up log to terminal
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.DEBUG)

# Set training parameters.
num_topics = 13
chunksize = 115521
passes = 20
iterations = 4000
eval_every = 1

# Make a index to word dictionary.
temp = dictionaryAll[0]  # This is only to "load" the dictionary.
id2word = dictionaryAll.id2token


model = LdaModel(
    corpus=corpus2018,
    id2word=id2word,
    chunksize=chunksize,
    alpha='auto',
    eta='auto',
    iterations=iterations,
    num_topics=num_topics,
    passes=passes,
    eval_every=eval_every
)

###################################################################################################################

for idx, topic in model.print_topics(-1,50):
    print("Topic: {} \nWords: {}".format(idx, topic ))
    print("\n")


# Calculate coherence
# coherence is: 0.35347 
coherence_model_lda = CoherenceModel(model=model, texts=justTweetsAll, dictionary=dictionaryAll, coherence='c_v')
coherence_lda = coherence_model_lda.get_coherence()
print('coherence is: ', coherence_lda)


# Visualize the topics
import pyLDAvis.gensim

pyLDAvis.enable_notebook()
vis = pyLDAvis.gensim.prepare(model, corpus2018, dictionaryAll, sort_topics=False)
pyLDAvis.show(vis)
pyLDAvis.save_html(vis, 'lda.html')


##################################################################################################################

#save for future use
#import pickle

#with open("model.pkl", "wb") as f:
#    pickle.dump(model, f, pickle.HIGHEST_PROTOCOL)

# Reload the file
#model = pickle.load(open("model.pkl", "rb"))

#also try this way as a backup:
model.save('C:/Users/matth/SocialStudio/data/model_save')
model = LdaModel.load('C:/Users/matth/SocialStudio/data/model_save')

#####################################################################################################################

txCorpus2018 = model[corpus2018]
txCorpus2019 = model[corpus2019]
txCorpus2020 = model[corpus2020]

sent_topics_df2018 = pd.DataFrame()

    # Get main topic in each document
for i, row in enumerate(txCorpus2018):
    row = sorted(row, key=lambda x: (x[1]), reverse=True)
    # Get the Dominant topic, Perc Contribution and Keywords for each document
    for j, (topic_num, prop_topic) in enumerate(row):
        if j == 0:  # => dominant topic
            sent_topics_df2018 = sent_topics_df2018.append(pd.Series([int(topic_num), round(prop_topic,4)]), ignore_index=True)
        else:
            break
sent_topics_df2018.columns = ['Dominant_Topic', 'Perc_Contribution']
posts2018.reset_index(drop=True, inplace=True)
processed2018 = pd.concat([posts2018, sent_topics_df2018], axis=1)


sent_topics_df2019 = pd.DataFrame()

    # Get main topic in each document
for i, row in enumerate(txCorpus2019):
    row = sorted(row, key=lambda x: (x[1]), reverse=True)
    # Get the Dominant topic, Perc Contribution and Keywords for each document
    for j, (topic_num, prop_topic) in enumerate(row):
        if j == 0:  # => dominant topic
            sent_topics_df2019 = sent_topics_df2019.append(pd.Series([int(topic_num), round(prop_topic,4)]), ignore_index=True)
        else:
            break
sent_topics_df2019.columns = ['Dominant_Topic', 'Perc_Contribution']
posts2019.reset_index(drop=True, inplace=True)
processed2019 = pd.concat([posts2019, sent_topics_df2019], axis=1)


sent_topics_df2020 = pd.DataFrame()

    # Get main topic in each document
for i, row in enumerate(txCorpus2020):
    row = sorted(row, key=lambda x: (x[1]), reverse=True)
    # Get the Dominant topic, Perc Contribution and Keywords for each document
    for j, (topic_num, prop_topic) in enumerate(row):
        if j == 0:  # => dominant topic
            sent_topics_df2020 = sent_topics_df2020.append(pd.Series([int(topic_num), round(prop_topic,4)]), ignore_index=True)
        else:
            break
sent_topics_df2020.columns = ['Dominant_Topic', 'Perc_Contribution']
posts2020.reset_index(drop=True, inplace=True)
processed2020 = pd.concat([posts2020, sent_topics_df2020], axis=1)

processed2018.to_csv("processedtweets2018.csv", index=False, encoding='utf-8-sig')
processed2019.to_csv("processedtweets2019.csv", index=False, encoding='utf-8-sig')
processed2020.to_csv("processedtweets2020.csv", index=False, encoding='utf-8-sig')
