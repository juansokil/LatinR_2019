
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 20 21:19:06 2018

@author: Juan
"""

import pandas as pd
import numpy as np
import re, nltk, spacy, gensim
from pprint import pprint
from sklearn.utils import shuffle
from spacy.lang.en.stop_words import STOP_WORDS

# spacy for lemmatization
import spacy
# Initialize spacy 'en' model, keeping only tagger component (for efficiency)
nlp = spacy.load('en_core_web_sm')

# Sklearn
from sklearn.decomposition import LatentDirichletAllocation, TruncatedSVD
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.model_selection import GridSearchCV

# Gensim
import gensim
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel
from gensim import models
from gensim.models import ldaseqmodel
from gensim.corpora import Dictionary
from gensim.matutils import hellinger

# Plotting tools
import pyLDAvis
import pyLDAvis.sklearn
import matplotlib.pyplot as plt
import pyLDAvis.gensim  # don't skip this

# Enable logging for gensim - optional
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.ERROR)
import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)







##############Levanta datos#####################3
base_genero = pd.read_csv("C:/Users/Juan/Dropbox/LatinR_2019/01-Bases/base_completa.txt", sep='\t', encoding='latin1')
base_genero = base_genero.set_index('UT')
#base_genero=base_genero.head(n=200)


###convierte el df a df
#base_genero.to_csv('C:/Users/Juan/Dropbox/LatinR_2019/01-Bases/base_reduce.txt',  header=True, sep='\t', encoding='latin1')




##Crea variable titulo + abstract
base_genero['Resumen'] = base_genero.TI.astype(str).str.cat(base_genero.AB.astype(str), sep=' ')
base_genero['Resumen'] = base_genero['Resumen'].str.lower()  


#################PARTE 1#####################
################FUNCION DE TGOKENIZAR##########
def sent_to_words(sentences):
    for sentence in sentences:
        yield(gensim.utils.simple_preprocess(str(sentence), deacc=True))  # deacc=True removes punctuations

################FUNCION DE LEMATIZAR##########
def lemmatization(texts, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV']):
    """https://spacy.io/api/annotation"""
    texts_out = []
    for sent in texts:
        doc = nlp(" ".join(sent)) 
        texts_out.append(" ".join([token.lemma_ if token.lemma_ not in ['-PRON-'] else '' for token in doc if token.pos_ in allowed_postags]))
    return texts_out


###############APLICO LA FUNCION DE TOKENS###################
data_words = list(sent_to_words(base_genero['Resumen']))
data_words_se = pd.Series(data_words)
base_genero['data_words'] = data_words_se.values

###############APLICO LA FUNCION DE LEMATIZAR###################
data_lemmatized = lemmatization(data_words, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV'])
data_lemmatized_se = pd.Series(data_lemmatized)
base_genero['data_lemmatized'] = data_lemmatized_se.values



###convierte el df a df
#base_genero.to_csv('C:/Users/Juan/Dropbox/LatinR_2019/01-Bases/base_pre_proc.csv',  header=True, sep='\t', encoding='latin1')
###Levanta datos
#base_genero = pd.read_csv("C:/Users/Juan/Dropbox/LatinR_2019/01-Bases/base_pre_proc.csv", sep='\t', encoding='latin1')
#base_genero = base_genero.set_index('UT')


################AGREGA STOPWORDS DEL DOMINIO #######################################3

stopwords_dominio = ["(c)", "abstract", "analysis", "approach", "argue", "article", "author", "compare", "conclude", "conclude", "conclusion", 
 "copyright", "discuss", "discussion", "elsevier", "emerald", "examine", "explore", "explore", "find", "franci", "francis", 
 "group", "hypothesis", "idea", "impact", "implications", "inc", "informa", "introduction", "limit", "limitations", "limited", 
 "llc", "ltd", "materials", "measure", "measure", "methodology", "methods", "objective", "paper", "predict", "prediction", 
 "present", "publication", "publications", "publishing", "purpose", "report", "research", "research", "result", "results", 
 "sage", "sample", "springer", "study", "studying", "survey", "taylor", "theory", "thesis", "understand"]  

for word in stopwords_dominio:
    STOP_WORDS.add(word)

base_genero['data_lemmatized_stopwords'] = base_genero['data_lemmatized'].apply(lambda x: ' '.join([word for word in x.split() if word not in (STOP_WORDS)]))

###ELIMINO EL COPYRIGHT
#copyright=[]
#i=1
#for i in range(len(base_genero)):
#    copyright.append(re.sub('â©.*', '',base_genero['Resumen_stopwords_dominio'][i]))
###convierto la lista en una serie
#se = pd.Series(copyright)
###asigno los valores de la serie a una nueva variable
#base_genero['Resumen_stopwords_dominio2'] = se.values



######################EN ESTE PASO VUELVO A HACER LA TOKENIZACION CON LOS DATOS FINALES#########
data_words = list(sent_to_words(base_genero['data_lemmatized_stopwords']))


###################### CONSTRUCCION DE N-GRAMAS#######
bigram = gensim.models.Phrases(data_words, min_count=5, threshold=0.1) # higher threshold fewer phrases.
trigram = gensim.models.Phrases(bigram[data_words], threshold=0.075)  
cuatrigram = gensim.models.Phrases(trigram[bigram[data_words]], threshold=0.05)  

# Faster way to get a sentence clubbed as a trigram/bigram
bigram_mod = gensim.models.phrases.Phraser(bigram)
trigram_mod = gensim.models.phrases.Phraser(trigram)
cuatrigram_mod = gensim.models.phrases.Phraser(cuatrigram)


def make_bigrams(texts):
    return [bigram_mod[doc] for doc in texts]

def make_trigrams(texts):
    return [trigram_mod[bigram_mod[doc]] for doc in texts]

def make_cuatrigrams(texts):
    return [cuatrigram_mod[trigram_mod[bigram_mod[doc]]] for doc in texts]



# Form Trigrams


make_cuatrigrams = make_cuatrigrams(data_words)
make_cuatrigrams_se = pd.Series(make_cuatrigrams)
base_genero['ngramas'] = make_cuatrigrams_se.values


##################################PASOS DICCIONARIO########################

# Create Dictionary
id2word = corpora.Dictionary(base_genero['ngramas'])
print(len(id2word))

id2word.filter_extremes(no_below=5)
id2word.compactify()
print(len(id2word))
#plt.bar(id2word.keys(),id2word.values())

# Create Corpus
texts = base_genero['ngramas']
# Term Document Frequency - Diccionario
corpus = [id2word.doc2bow(text) for text in texts]


# View
print(corpus[:1])
print(len(corpus))

# Human readable format of corpus (term-frequency)
[[(id2word[id], freq) for id, freq in cp] for cp in corpus[:1]]



#dict=pd.DataFrame.from_dict(corpus)
#dict.to_csv('../base_completa/diccionario.csv',  header=True, sep='\t', encoding='latin1')


###############################Resumen de etapas##############################



# Enable logging for gensim - optional
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.ERROR)
import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)


####################LDA##########
################ARMO EL MEJOR MODELO#####
np.random.seed(1977)
num_topics=100

eta=(200/len(id2word))
#alpha=50/num_topics





#passes: Number of passes through the entire corpus
#chunksize: Number of documents to load into memory at a time and process E step of EM.
#update_every: number of chunks to process prior to moving onto the M step of EM.


lda_model

#vocabulary=38488
# Build LDA model
lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,id2word=id2word,num_topics=num_topics,random_state=1982, update_every=2, chunksize=1000, iterations=1000, passes=10, alpha='auto',  eta=eta)
#lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,id2word=id2word,num_topics=num_topics,random_state=100,update_every=1,chunksize=1000, iterations=1000, passes=10, alpha=0.01,  eta=eta)
#lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,id2word=id2word,num_topics=num_topics,random_state=100,update_every=1,chunksize=1000, iterations=1000, passes=10, offset=64, alpha=(50/num_topics),  decay=0.5, eta=(200/len(id2word)))

###learning decay 0.5 (es el mejor, probado en el paper de Blei)
###alpha=0,1 (con un alfa pequeño logramos que los documentos, que pasa si pongo default)
    
#Griffiths TL, Steyvers M (2004). “Finding Scientific Topics.” Proceedings of the National Academy of Sciences of the United States of America, 101, 5228–5235.
##ACA PROPONEN LA CANTIDAD DE TOPICOS

####lambda## aca puedo obtener la probabilidad de cada palabra para cada topico
topics_terms = lda_model.state.get_lambda() 
#convert estimates to probability (sum equals to 1 per topic)
#topics_terms_proba = np.apply_along_axis(lambda x: x/x.sum(),1,topics_terms)
#topics_terms_proba = pd.DataFrame(topics_terms_proba)
#topics_terms_proba.to_csv('../resultados/topics_terms_proba.csv')


# find the right word based on column index
words = [lda_model.id2word[i] for i in range(topics_terms.shape[1])]
#put everything together
#topic_palabra=pd.DataFrame(topics_terms,columns=words)
len(words)



lda_model.get_document_topics
doc_lda = lda_model[corpus]



print (lda_model.print_topics())


###Hago un archivo con las palabras 
top_words_per_topic = []
for t in range(lda_model.num_topics):
    top_words_per_topic.extend([(t, ) + x for x in lda_model.show_topic(t, topn = 30)])

#pd.DataFrame(top_words_per_topic, columns=['Topic', 'Word', 'P']).to_csv("../resultados/top_words.csv")


###Hago un archivo con el total del diccionario
top_words_per_topic = []
for t in range(lda_model.num_topics):
    top_words_per_topic.extend([(t, ) + x for x in lda_model.show_topic(t, topn = len(id2word))])

#pd.DataFrame(top_words_per_topic, columns=['Topic', 'Word', 'P']).to_csv("../resultados/total_words.csv")
top_words=pd.DataFrame(top_words_per_topic, columns=['Topic', 'Word', 'P'])
table = pd.pivot_table(top_words, values='P', index=['Word'],columns=['Topic'], aggfunc=np.sum)
#table.to_csv("../resultados/tabla_total.csv")













#############ARMA TOPICOS - PROBABILIDADES#####
lda_model.n_topics=num_topics
# column names
topicnames = ["Topic" + str(i) for i in range(lda_model.n_topics)]
rango=len(base_genero)
# index names
docnames = ["Doc" + str(i) for i in range(rango)]

#####Get document - topic values
theta, _ = lda_model.inference(corpus)
theta /= theta.sum(axis=1)[:, None]
# Make the pandas dataframe
df_document_topic = pd.DataFrame(np.round(theta,3), columns=topicnames, index=docnames)
# Get dominant topic for each document
dominant_topic = np.argmax(df_document_topic.values, axis=1)
df_document_topic['dominant_topic'] = dominant_topic

df_document_topic=df_document_topic.reset_index()
#datos = base_genero[['Año','Pais','Resumen_ok']]
#datos['Pais'].str.strip()
#datos=datos.reset_index()

#base_analisis=pd.concat([df_document_topic, datos], ignore_index=True, axis=1)
#base_analisis.columns = ['doc', 'topico01', 'topico02', 'topico03', 'topico04', 'topico05', 'topico06'
#                         , 'topico07', 'topico08', 'topico09', 'topico10','topicodom','ut','Año','Pais']



df_topic_distribution = df_document_topic['dominant_topic'].value_counts().reset_index(name="Num Documents")
df_topic_distribution.columns = ['Topic Num', 'Num Documents']
df_topic_distribution



valores_alpha=lda_model.alpha
valores_beta=lda_model.eta
plt.plot(valores_alpha)

pyLDAvis.enable_notebook()
vis = pyLDAvis.gensim.prepare(lda_model, corpus, id2word, sort_topics=False)
datos_tsne=vis[1]
posiciones=vis[0]

datos_tsne['Relevance']= (datos_tsne['loglift']*0.4) + (datos_tsne['logprob']*0.6)  
datos_tsne.to_csv('../resultados/datos_tsne.csv',  header=True, sep='\t', encoding='latin1')
posiciones.to_csv('../resultados/posiciones.csv',  header=True, sep='\t', encoding='latin1')


####save LDA davis####
pyLDAvis.save_html(vis,'../resultados/vis_gensim.html')

###Save LDA model####
lda_model.save('../resultados/lda_model')


Agrupados= top_words.groupby('Topic')['Word'].apply(', '.join).reset_index()
Agrupados.to_csv('../resultados/palabras.csv',  header=True, sep='\t', encoding='latin1')

Agrupados2= datos_tsne.sort_values(['Relevance'], ascending=False).groupby('Category')['Term'].apply(', '.join).reset_index()
Agrupados2.to_csv('../resultados/palabras2.csv',  header=True, sep='\t', encoding='latin1')




###Levanta datos
#base_analisis = pd.read_csv("../resultados/base_topicos.csv", sep='\t', encoding='latin1')
#base_analisis=base_analisis.drop(columns=['Pais'])
codigo_pais = pd.read_csv("../base_completa/identifica_pais.txt", sep='\t', encoding='latin1')
resultados = base_analisis.merge(codigo_pais, left_on='ut', right_on='ut', how='outer')
base_analisis=resultados
###convierte el df a df
base_analisis.to_csv('../resultados/base_topicos.csv',  header=True, sep='\t', encoding='latin1')















