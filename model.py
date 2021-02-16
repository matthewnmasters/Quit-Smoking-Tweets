#bring in all the data and create common dictionary, etc

import os
import nltk
os.chdir('C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets')


import pandas as pd

#bring in the single file. Dates with Pandas and in Python in general are even more confusing than R
allYears = pd.read_csv('noRedactedTweets.csv')
newTweets = pd.read_csv('1102_1231_2020.csv', encoding='utf-8-sig')

data2018 = allYears[allYears["PUBLISH_DATE"].str.contains("2018") == True]
data2019 = allYears[allYears["PUBLISH_DATE"].str.contains("2019") == True]
data2020 = allYears[allYears["PUBLISH_DATE"].str.contains("2020") == True]
newData = newTweets[newTweets["PUBLISH_DATE"].str.contains("2020") == True]

#make tweets all lowercase here, rather than later because we can filter it easier as a data frame
data2018["CONTENT"] = data2018["CONTENT"].str.lower()
data2019["CONTENT"] = data2019["CONTENT"].str.lower()
data2020["CONTENT"] = data2020["CONTENT"].str.lower()
newData["CONTENT"] = newData["CONTENT"].str.lower()

#remove all URLs from tweets to facilitate removing of spam/bot posts of almost identical tweets
#could this result in blank tweets that cause a problem?
import re
data2018['CONTENT'] = data2018['CONTENT'].apply(lambda x: re.sub(r'http\S+', '', x))
data2019['CONTENT'] = data2019['CONTENT'].apply(lambda x: re.sub(r'http\S+', '', x))
data2020['CONTENT'] = data2020['CONTENT'].apply(lambda x: re.sub(r'http\S+', '', x))
newData['CONTENT'] = newData['CONTENT'].apply(lambda x: re.sub(r'http\S+', '', x))

#remove tweets that contain "weed"
data2018 = data2018[data2018["CONTENT"].str.contains("weed") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("weed") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("weed") == False]
newData = newData[newData["CONTENT"].str.contains("weed") == False]

#remove tweets that contain "blunt"
data2018 = data2018[data2018["CONTENT"].str.contains("blunt") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("blunt") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("blunt") == False]
newData = newData[newData["CONTENT"].str.contains("blunt") == False]

#remove tweets that contain "crack"
data2018 = data2018[data2018["CONTENT"].str.contains("crack") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("crack") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("crack") == False]
newData = newData[newData["CONTENT"].str.contains("crack") == False]

#remove tweets that contain "roach"
data2018 = data2018[data2018["CONTENT"].str.contains("roach") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("roach") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("roach") == False]
newData = newData[newData["CONTENT"].str.contains("roach") == False]

#remove tweets that contain "baked"
data2018 = data2018[data2018["CONTENT"].str.contains("baked") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("baked") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("baked") == False]
newData = newData[newData["CONTENT"].str.contains("baked") == False]

#remove tweets that contain "dabs"
data2018 = data2018[data2018["CONTENT"].str.contains("dabs") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("dabs") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("dabs") == False]
newData = newData[newData["CONTENT"].str.contains("dabs") == False]

#remove tweets that contain "marijuana"
data2018 = data2018[data2018["CONTENT"].str.contains("marijuana") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("marijuana") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("marijuana") == False]
newData = newData[newData["CONTENT"].str.contains("marijuana") == False]

#remove tweets that contain "bong"
data2018 = data2018[data2018["CONTENT"].str.contains("bong") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("bong") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("bong") == False]
newData = newData[newData["CONTENT"].str.contains("bong") == False]

#remove tweets that contain "mary jane"
data2018 = data2018[data2018["CONTENT"].str.contains("mary jane") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("mary jane") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("mary jane") == False]
newData = newData[newData["CONTENT"].str.contains("mary jane") == False]

#remove tweets that contain "maryjane"
data2018 = data2018[data2018["CONTENT"].str.contains("maryjane") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("maryjane") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("maryjane") == False]
newData = newData[newData["CONTENT"].str.contains("maryjane") == False]

#remove tweets that contain "pot"
data2018 = data2018[data2018["CONTENT"].str.contains("pot") == False]
data2019 = data2019[data2019["CONTENT"].str.contains("pot") == False]
data2020 = data2020[data2020["CONTENT"].str.contains("pot") == False]
newData = newData[newData["CONTENT"].str.contains("pot") == False]

######################################################################################################
#Create dataframes for each "type" of tweet

#only original posts:
posts2018 = data2018[data2018.POST_TYPE.isnull()]
posts2019 = data2019[data2019.POST_TYPE.isnull()]
posts2020 = data2020[data2020.POST_TYPE.isnull()]
newDataPosts = newData[newData.POST_TYPE.isnull()]

#only retweets:
retweets2018 = data2018[data2018.POST_TYPE.eq("RETWEET")]
retweets2019 = data2019[data2019.POST_TYPE.eq("RETWEET")]
retweets2020 = data2020[data2020.POST_TYPE.eq("RETWEET")]
newDataRetweets = newData[newData.POST_TYPE.eq("RETWEET")]

#only quote tweets:
quoteTweets2018 = data2018[data2018.POST_TYPE.eq("QUOTE_TWEET")]
quoteTweets2019 = data2019[data2019.POST_TYPE.eq("QUOTE_TWEET")]
quoteTweets2020 = data2020[data2020.POST_TYPE.eq("QUOTE_TWEET")]
newDataQuoteTweets = newData[newData.POST_TYPE.eq("QUOTE_TWEET")]
  
#quote tweets AND retweets:
nonOriginaltweets2018 = data2018[data2018.POST_TYPE.notnull()]
nonOriginaltweets2019 = data2019[data2019.POST_TYPE.notnull()]
nonOriginaltweets2020 = data2020[data2020.POST_TYPE.notnull()]
newDataNonOriginal = newData[newData.POST_TYPE.notnull()]


#################################################################################################################
#Now we are ready to just look at processing the personal tweets, not retweets or quotes.
#################################################################################################################

#remove duplicate posts from different authors. We already removed dupes from the same authors, but theres lots of bot spam still
posts2018 = posts2018.drop_duplicates(subset=['CONTENT'])
posts2019 = posts2019.drop_duplicates(subset=['CONTENT'])
posts2020 = posts2020.drop_duplicates(subset=['CONTENT'])
newDataPosts = newDataPosts.drop_duplicates(subset=['CONTENT'])

#132558-115521 , 109688-103166 , 108237-103979
#Create a list of tweets instead of a data frame

justTweets2018 = list(posts2018['CONTENT'])
justTweets2019 = list(posts2019['CONTENT'])
justTweets2020 = list(posts2020['CONTENT'])
justTweetsNEW = list(newDataPosts['CONTENT'])

#POS tag then lemmatize. This is slow but I've had trouble using pos_tag_sents and converting those tupples to wordnet tags.
#It's really weird how the nltk tags aren't compatible. Annoying, but this doesnt take TOO long, at least. Will need to spend
#some time optimizing this if we ever do it with a larger data set (in the millions)
from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet
from nltk.tokenize import RegexpTokenizer

lemmatizer = WordNetLemmatizer()
tokenizer = RegexpTokenizer(r'\w+')

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
justTweetsNEW = [lemmatize_sentence(doc) for doc in justTweetsNEW]

#We need to lemmatize again. Had trouble making the code return a list. Still learning...

for idx in range(len(justTweets2018)):
    justTweets2018[idx] = tokenizer.tokenize(justTweets2018[idx])  # Split into words.
    
for idx in range(len(justTweets2019)):
    justTweets2019[idx] = tokenizer.tokenize(justTweets2019[idx])  # Split into words.

for idx in range(len(justTweets2020)):
    justTweets2020[idx] = tokenizer.tokenize(justTweets2020[idx])  # Split into words.
    
for idx in range(len(justTweetsNEW)):
    justTweetsNEW[idx] = tokenizer.tokenize(justTweetsNEW[idx])  # Split into words.



# Remove numbers, but not words that contain numbers.
justTweets2018 = [[token for token in doc if not token.isnumeric()] for doc in justTweets2018]
justTweets2019 = [[token for token in doc if not token.isnumeric()] for doc in justTweets2019]
justTweets2020 = [[token for token in doc if not token.isnumeric()] for doc in justTweets2020]
justTweetsNEW = [[token for token in doc if not token.isnumeric()] for doc in justTweetsNEW]

# Remove words that are only one character. #changed to more than two characters, do we want to consider stop words?
justTweets2018 = [[token for token in doc if len(token) > 2] for doc in justTweets2018]
justTweets2019 = [[token for token in doc if len(token) > 2] for doc in justTweets2019]
justTweets2020 = [[token for token in doc if len(token) > 2] for doc in justTweets2020]
justTweetsNEW = [[token for token in doc if len(token) > 2] for doc in justTweetsNEW]


# Remove stop words
from nltk.corpus import stopwords
#nltk.download('stopwords')
justTweets2018 = [[token for token in doc if token not in stopwords.words('english')] for doc in justTweets2018]
justTweets2019 = [[token for token in doc if token not in stopwords.words('english')] for doc in justTweets2019]
justTweets2020 = [[token for token in doc if token not in stopwords.words('english')] for doc in justTweets2020]
justTweetsNEW = [[token for token in doc if token not in stopwords.words('english')] for doc in justTweetsNEW]

#################################################################################################################

# Compute bigrams. Currently this adds the bigram to the end, but that introduces bias. maybe just keep the bigram version
from gensim.models import Phrases

# Add bigrams to docs (only ones that appear 20 times or more). May want to repeat it for trigrams
bigram2018 = Phrases(justTweets2018, min_count=20)
bigram2019 = Phrases(justTweets2019, min_count=20)
bigram2020 = Phrases(justTweets2020, min_count=20)
bigramNEW = Phrases(justTweetsNEW, min_count=20)

justTweets2018= list(bigram2018[justTweets2018])
justTweets2019= list(bigram2019[justTweets2019])
justTweets2020= list(bigram2020[justTweets2020])
justTweetsNEW= list(bigramNEW[justTweetsNEW])

justTweetsAll= justTweets2018 + justTweets2019 + justTweets2020
#Dont add the new tweets because we want to recreate the same dictionary

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
corpusNEW = [dictionaryAll.doc2bow(doc) for doc in justTweetsNEW]

# Let's see how many tokens and documents we have to train on
print('Number of unique tokens: %d' % len(dictionaryAll))
print('Number of documents: %d' % len(corpusAll))

##################################################################################################################

#monte carlo simulation to identify number of topics

from gensim.models import LdaModel
from gensim.models import CoherenceModel


# Set training parameters.
chunksize = 67479
passes = 10
iterations = 200
eval_every = None  # evaluate perplexity

# Make a index to word dictionary.
temp = dictionaryAll[0]  # This is only to "load" the dictionary.
id2word = dictionaryAll.id2token

#montecarlo showed 8 topics as being a decent spot for c_v coherence
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
num_topics = 10
chunksize = 67479
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
# coherence is: 0.33
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
model.save('C:/Users/matth/Documents/GitHub/QS2020/model_save')
model = LdaModel.load('C:/Users/matth/Documents/GitHub/QS2020/model_save')

#####################################################################################################################



#trying to not just pull the most dominant topic, but all of them so we can give partial topic counts
wide2018 = pd.DataFrame(model.get_document_topics(corpus2018))
wide2018.columns = ['zero','one','two','three','four','five','six','seven','eight','nine']
wide2019 = pd.DataFrame(model.get_document_topics(corpus2019))
wide2019.columns = ['zero','one','two','three','four','five','six','seven','eight','nine']
wide2020 = pd.DataFrame(model.get_document_topics(corpus2020))
wide2020.columns = ['zero','one','two','three','four','five','six','seven','eight','nine']
wideNEW = pd.DataFrame(model.get_document_topics(corpusNEW))
wideNEW.columns = ['zero','one','two','three','four','five','six','seven','eight','nine']


#the above code gives tuples, we want them to be individual columns
wide2018['zero_top'], wide2018['zero_perc'] =wide2018.zero.str
wide2018['one_top'], wide2018['one_perc'] =wide2018.one.str
wide2018['two_top'], wide2018['two_perc'] =wide2018.two.str
wide2018['three_top'], wide2018['three_perc'] =wide2018.three.str
wide2018['four_top'], wide2018['four_perc'] =wide2018.four.str
wide2018['five_top'], wide2018['five_perc'] =wide2018.five.str
wide2018['six_top'], wide2018['six_perc'] =wide2018.six.str
wide2018['seven_top'], wide2018['seven_perc'] =wide2018.seven.str
wide2018['eight_top'], wide2018['eight_perc'] =wide2018.eight.str
wide2018['nine_top'], wide2018['nine_perc'] =wide2018.nine.str

wide2019['zero_top'], wide2019['zero_perc'] =wide2019.zero.str
wide2019['one_top'], wide2019['one_perc'] =wide2019.one.str
wide2019['two_top'], wide2019['two_perc'] =wide2019.two.str
wide2019['three_top'], wide2019['three_perc'] =wide2019.three.str
wide2019['four_top'], wide2019['four_perc'] =wide2019.four.str
wide2019['five_top'], wide2019['five_perc'] =wide2019.five.str
wide2019['six_top'], wide2019['six_perc'] =wide2019.six.str
wide2019['seven_top'], wide2019['seven_perc'] =wide2019.seven.str
wide2019['eight_top'], wide2019['eight_perc'] =wide2019.eight.str
wide2019['nine_top'], wide2019['nine_perc'] =wide2019.nine.str

wide2020['zero_top'], wide2020['zero_perc'] =wide2020.zero.str
wide2020['one_top'], wide2020['one_perc'] =wide2020.one.str
wide2020['two_top'], wide2020['two_perc'] =wide2020.two.str
wide2020['three_top'], wide2020['three_perc'] =wide2020.three.str
wide2020['four_top'], wide2020['four_perc'] =wide2020.four.str
wide2020['five_top'], wide2020['five_perc'] =wide2020.five.str
wide2020['six_top'], wide2020['six_perc'] =wide2020.six.str
wide2020['seven_top'], wide2020['seven_perc'] =wide2020.seven.str
wide2020['eight_top'], wide2020['eight_perc'] =wide2020.eight.str
wide2020['nine_top'], wide2020['nine_perc'] =wide2020.nine.str

wideNEW['zero_top'], wideNEW['zero_perc'] =wideNEW.zero.str
wideNEW['one_top'], wideNEW['one_perc'] =wideNEW.one.str
wideNEW['two_top'], wideNEW['two_perc'] =wideNEW.two.str
wideNEW['three_top'], wideNEW['three_perc'] =wideNEW.three.str
wideNEW['four_top'], wideNEW['four_perc'] =wideNEW.four.str
wideNEW['five_top'], wideNEW['five_perc'] =wideNEW.five.str
wideNEW['six_top'], wideNEW['six_perc'] =wideNEW.six.str
wideNEW['seven_top'], wideNEW['seven_perc'] =wideNEW.seven.str
wideNEW['eight_top'], wideNEW['eight_perc'] =wideNEW.eight.str
wideNEW['nine_top'], wideNEW['nine_perc'] =wideNEW.nine.str

#Now merge with the original data
posts2018.reset_index(drop=True, inplace=True)
processedWide2018 = pd.concat([posts2018, wide2018], axis=1)

posts2019.reset_index(drop=True, inplace=True)
processedWide2019 = pd.concat([posts2019, wide2019], axis=1)

posts2020.reset_index(drop=True, inplace=True)
processedWide2020 = pd.concat([posts2020, wide2020], axis=1)

newDataPosts.reset_index(drop=True, inplace=True)
processedWideNEW = pd.concat([newDataPosts, wideNEW], axis=1)


processedWide2018.to_csv("processedWide2018.csv", index=False, encoding='utf-8-sig')
processedWide2019.to_csv("processedWide2019.csv", index=False, encoding='utf-8-sig')
processedWide2020.to_csv("processedWide2020.csv", index=False, encoding='utf-8-sig')
processedWideNEW.to_csv("processedWideNEW.csv", index=False, encoding='utf-8-sig')



#Leaving to show how just the dominant topic was extracted previously.

# txCorpus2018 = model[corpus2018]
# txCorpus2019 = model[corpus2019]
# txCorpus2020 = model[corpus2020]
# txCorpusNEW = model[corpusNEW]

# sent_topics_dfNEW = pd.DataFrame()

#     # Get main topic in each document
# for i, row in enumerate(txCorpusNEW):
#     row = sorted(row, key=lambda x: (x[1]), reverse=True)
#     # Get the Dominant topic, Perc Contribution and Keywords for each document
#     for j, (topic_num, prop_topic) in enumerate(row):
#         if j == 0:  # => dominant topic
#             sent_topics_dfNEW = sent_topics_dfNEW.append(pd.Series([int(topic_num), round(prop_topic,4)]), ignore_index=True)
#         else:
#             break
# sent_topics_dfNEW.columns = ['Dominant_Topic', 'Perc_Contribution']
# newDataPosts.reset_index(drop=True, inplace=True)
# processedNEW = pd.concat([newDataPosts, sent_topics_dfNEW], axis=1)


# processed2018.to_csv("processedtweets2018.csv", index=False, encoding='utf-8-sig')
# processed2019.to_csv("processedtweets2019.csv", index=False, encoding='utf-8-sig')
# processed2020.to_csv("processedtweets2020.csv", index=False, encoding='utf-8-sig')
# processedNEW.to_csv("processedtweetsNEW.csv", index=False, encoding='utf-8-sig')
