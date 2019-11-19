# -*- coding: utf-8 -*-
"""
Created on Sat Oct 26 15:08:11 2019

@author: Juan
"""

import pandas as pd

# spacy for lemmatization
import spacy
# Initialize spacy 'en' model, keeping only tagger component (for efficiency)
nlp = spacy.load('en_core_web_sm')

# Gensim
import gensim
import gensim.corpora as corpora

# Enable logging for gensim - optional
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.ERROR)
import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)

# Save objects
import pickle



base_genero = pd.read_csv("C:/Users/Juan/Dropbox/LatinR_2019/01-Bases/base_completa_reducida.txt", sep='\t', encoding='utf-8')
base_genero=base_genero.head(100)


#pasa a minuscula
base_genero['AB'] = base_genero['AB'].str.lower()
#Largo de cada abstract
base_genero['AB'].str.len()

##Crea variable titulo + abstract
base_genero['Resumen'] = base_genero.TI.astype(str).str.cat(base_genero.AB.astype(str), sep=' ')
base_genero['Resumen'] = base_genero['Resumen'].str.lower()

"""Genero las funciones para procesar el texto"""

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


"""Aplico la función de Tokens"""

###############APLICO LA FUNCION DE TGOKENIZAR###################
data_words = list(sent_to_words(base_genero['Resumen']))
data_words_se = pd.Series(data_words)
#base_genero['data_words'] = data_words_se.values
print(data_words[2])

"""Lematizar los resumenes manteniendo solo: Noun, Adj, Verb, Adverb"""

###############APLICO LA FUNCION DE LEMATIZAR###################
data_lemmatized = lemmatization(data_words, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV'])
data_lemmatized_se = pd.Series(data_lemmatized)
base_genero['data_lemmatized'] = data_lemmatized_se.values
print(data_lemmatized[2])

"""Importa las Stopwords y agrega las necesarias"""

####GENERALES####
from spacy.lang.en.stop_words import STOP_WORDS

####DOMINIO####
stopwords_dominio = ['(c)', 'abstract', 'aim', 'analysis', 'analize', 'approach', 'argue', 'article', 'author', 'authors', 
                     'background', 'compare', 'conclude', 'conclusion', 'conclusions', 'context', 'copyright', 'data', 'de', 
                     'dept', 'discuss', 'discussed', 'discussion', 'elsevier', 'emerald', 'examine', 'examined', 'experience', 
                     'experienced', 'explore', 'finding', 'findings', 'find', 'focus', 'focused', 'franci', 'francis', 'group', 
                     'hypothesis',  'idea', 'impact', 'implications', 'inc', 'informa', 'information', 'inform', 'interview', 
                     'introduction', 'journal', 'la', 'limit', 'limitations', 'limited', 'literature', 'llc', 'ltd', 
                     'materials', 'measure', 'measured', 'methodology', 'method', 'methods', 'model', 'na', 'objective', 
                     'paper', 'predict', 'prediction', 'present', 'publication', 'publications', 'published', 'publishing', 
                     'purpose', 'qualitative', 'quantitative', 'related', 'report', 'reported', 'research', 'researched', 
                     'result', 'results', 'sage', 'sample', 'springer', 'studies', 'study', 'studying', 'suggest', 'survey', 
                     'taylor', 'theory', 'thesis', 'understand' 'university', 'univ']  


####PAISES####
stopwords_countries = ['Africa', 'Europe', 'Asia', 'America', 'Oceania', 'Afghanistan',  'Albania',  'Algeria',  'Andorra',  'Angola',  'Antigua and Barbuda',  'Argentina', 
                       'Armenia',  'Australia',  'Austria',  'Azerbaijan',  'Bahamas',  'Bahrain',  'Bangladesh',  'Barbados',  
                       'Belarus',  'Belgium',  'Belize',  'Benin',  'Bhutan',  'Bolivia',  'Bosnia and Herzegovina',  
                       'Botswana',  'Brazil',  'Brunei',  'Bulgaria',  'Burkina Faso',  'Burundi',  "Côte d'Ivoire",  
                       'Cabo Verde',  'Cambodia',  'Cameroon',  'Canada',  'Central African Republic',  'Chad',  'Chile',  
                       'China',  'Colombia',  'Comoros',  'Congo (Congo-Brazzaville)',  'Congo Republic', 'Costa Rica',  'Croatia',  'Cuba',  'Cyprus',  'Czechia',  'Democratic Republic of the Congo',  'Denmark',  'Djibouti',  'Dominica',  'Dominican Republic',  'Ecuador',  'Egypt',  'El Salvador',  'Equatorial Guinea',  'Eritrea',  'Estonia',  'Ethiopia',  'Fiji',  'Finland',  'France',  'Gabon',  'Gambia',  'Georgia',  'Germany',  'Ghana',  'Greece',  'Grenada',  'Guatemala',  'Guinea',  'Guinea-Bissau',  'Guyana',  'Haiti',  'Holy See',  'Honduras',  'Hungary',  'Iceland',  'India',  'Indonesia',  'Iran',  'Iraq',  'Ireland',  'Israel',  'Italy',  'Jamaica',  'Japan',  'Jordan',  'Kazakhstan',  'Kenya',  'Kiribati',  'Kuwait',  'Kyrgyzstan',  'Laos',  'Latvia',  'Lebanon',  'Lesotho',  'Liberia',  'Libya',  'Liechtenstein',  'Lithuania',  'Luxembourg',  'Madagascar',  'Malawi',  'Malaysia',  'Maldives',  'Mali',  'Malta',  'Marshall Islands',  'Mauritania',  'Mauritius',  'Mexico',  'Micronesia',  'Moldova',  'Monaco',  'Mongolia',  'Montenegro',  'Morocco',  'Mozambique',  'Myanmar (formerly Burma)',  'Namibia',  'Nauru',  'Nepal',  'Netherlands',  'New Zealand',  'Nicaragua',  'Niger',  'Nigeria',  'North Korea',  'North Macedonia',  'Norway',  'Oman',  'Pakistan',  'Palau',  'Palestine State',  'Panama',  'Papua New Guinea',  'Paraguay',  'Peru',  'Philippines',  'Poland',  'Portugal',  'Qatar',  'Romania',  'Russia',  'Rwanda',  'Saint Kitts and Nevis',  'Saint Lucia',  'Saint Vincent and the Grenadines',  'Samoa',  'San Marino',  'Sao Tome and Principe',  'Saudi Arabia',  'Senegal',  'Serbia',  'Seychelles',  'Sierra Leone',  'Singapore',  'Slovakia',  'Slovenia',  'Solomon Islands',  'Somalia',  'South Africa',  'South Korea',  'South Sudan',  'Spain',  'Sri Lanka',  'Sudan',  'Suriname',  'Swaziland',  'Sweden',  'Switzerland',  'Syria',  'Tajikistan',  'Tanzania',  'Thailand',  'Timor-Leste',  'Togo',  'Tonga',  'Trinidad and Tobago',  'Tunisia',  'Turkey',  'Turkmenistan',  'Tuvalu',  'Uganda',  'Ukraine',  'United Arab Emirates',  'United Kingdom',  'United States of America',  'Uruguay',  'Uzbekistan',  'Vanuatu',  'Venezuela',  'Vietnam',  'Yemen',  'Zambia',  'Zimbabwe']  


for word in stopwords_dominio:
    STOP_WORDS.add(word)

for word in stopwords_countries:
    STOP_WORDS.add(word)
    
    
base_genero['data_lemmatized_stopwords'] = base_genero['data_lemmatized'].apply(lambda x: ' '.join([word for word in x.split() if word not in (STOP_WORDS)]))

base_genero.head()

"""Ya tengo el campo de texto preprocesado, ahora vuelvo a ejecutar el procedimiento final"""

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

"""Puedo listar uno de los casos para ver las palabras que tiene:"""

print('Cantidad de palabras: ', len(make_cuatrigrams[2]))
print(make_cuatrigrams[2])

"""Elimina los tokens generales que aparecen en mas del 20% de las publicaciones - Esto es una decisión teórica para no ensuciar los resultados posteriores"""

for i in range(len(make_cuatrigrams)):
    for word in make_cuatrigrams[i]:
        if 'gender' in make_cuatrigrams[i]:
            #print('Si')
            make_cuatrigrams[i].remove('gender')
        if 'woman' in make_cuatrigrams[i]:
            #print('Si')
            make_cuatrigrams[i].remove('woman')
        if 'female' in make_cuatrigrams[i]:
            #print('Si')
            make_cuatrigrams[i].remove('female')
        if 'social' in make_cuatrigrams[i]:
            #print('Si')
            make_cuatrigrams[i].remove('social')
        if 'difference' in make_cuatrigrams[i]:
            #print('Si')
            make_cuatrigrams[i].remove('difference')
            #print('Despues: ',len(make_cuatrigrams[i]))
        if 'american_psychological_association' in make_cuatrigrams[i]:
            #print('Si')
            make_cuatrigrams[i].remove('american_psychological_association')
            #print('Despues: ',len(make_cuatrigrams[i]))


"""Una vez que limpio las palabras muy frecuentes puedo ver las diferencias"""

print('Cantidad de palabras: ', len(make_cuatrigrams[2]))
print(make_cuatrigrams[2])

### Creo una variable que se llama n-gramas
make_cuatrigrams_se = pd.Series(make_cuatrigrams)
base_genero['ngramas'] = make_cuatrigrams_se.values


##################################PASOS DICCIONARIO########################
# Create Dictionary
id2word = corpora.Dictionary(base_genero['ngramas'])
print('Diccionario Inicial: ', len(id2word))



id2word.filter_extremes(no_below=5)
id2word.compactify()
print('Diccionario Filtrado: ', len(id2word))


# Create Corpus
texts = base_genero['ngramas']
# Term Document Frequency - Diccionario
bow = [id2word.doc2bow(text) for text in texts]

# Human readable format of corpus (term-frequency)
[[(id2word[id], freq) for id, freq in cp] for cp in bow[2:3]]


# Guardar base
base_genero = base_genero.drop(['AB', 'data_lemmatized', 'data_lemmatized_stopwords' ], axis=1)

corpus=bow

base_genero.to_csv('C:/Users/Juan/Dropbox/LatinR_2019/01-Bases/base_completa_lda.txt',  header=True, sep='\t', encoding='utf-8', line_terminator='\n')

###Guardar datos####
pickle.dump(id2word, open("C:/Users/Juan/Dropbox/LatinR_2019/04-Modelos/dictionary.pkl", "wb"))
pickle.dump(bow, open('C:/Users/Juan/Dropbox/LatinR_2019/04-Modelos/corpus.pkl', 'wb'))




import pyLDAvis.gensim
pyLDAvis.enable_notebook()


d = id2word
c = corpus
lda = lda_model

# Enable logging for gensim - optional
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.ERROR)
warnings.filterwarnings("ignore",category=DeprecationWarning)


vis = pyLDAvis.gensim.prepare(lda, c, d, sort_topics=False)
####save LDA davis####
pyLDAvis.save_html(vis,'vis_gensim.html')



datos_tsne=vis[1]
posiciones=vis[0]


datos_tsne['Relevance']= (datos_tsne['loglift']*0.4) + (datos_tsne['logprob']*0.6)  
datos_tsne.to_csv('../resultados/datos_tsne.csv',  header=True, sep='\t', encoding='latin1')
posiciones.to_csv('../resultados/posiciones.csv',  header=True, sep='\t', encoding='latin1')
