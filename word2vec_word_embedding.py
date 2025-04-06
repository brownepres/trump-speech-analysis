import pandas as pd
import gensim
from gensim.models import Word2Vec
import nltk
from nltk.tokenize import word_tokenize

df_tfidf = pd.read_csv('top_word_df.csv')
train_data = pd.read_csv("text_for_dtm.csv")

sentences = train_data['x']

word2vec = Word2Vec(sentences, vector_size=100, window=5, min_count=1, workers=4)
print("model has been trained")
# Save the model (optional)
word2vec.save("custom_word2vec.model")

def word_to_vec(word):
    if word in word2vec.wv:
        return word2vec.wv[word].tolist()  # Returns a NumPy array
    else:
        return None  # For words not in vocabulary

df_tfidf["Vector"] = df_tfidf["word"].apply(lambda words: [word_to_vec(w) for w in words])
df_tfidf["Vector"] = df_tfidf["Vector"].apply(lambda x: str(x) if x is not None else "None")

print(df_tfidf.head(5))

df_tfidf.to_csv("word2vec_vectors.csv")
