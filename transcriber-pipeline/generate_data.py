from pytubefix import YouTube
from pytubefix.cli import on_progress
from pyannote.audio import Pipeline
from pydub import AudioSegment
from transformers import AutoModelForSpeechSeq2Seq, AutoProcessor, pipeline
import torch
import os
import whisper
import warnings
import pandas as pd
import accelerate
import pprint 
pd.set_option('display.max_rows', 500)
warnings.filterwarnings('ignore')

#get video urls
speeches = pd.read_excel("speeches.xlsx")

#script to save audio files
def save_audio(url, output_path, filename):
    try:
        yt = YouTube(url)
        print(f"Downloading: {yt.title}")
        
        # Get audio-only stream
        ys = yt.streams.get_audio_only()
        
        # Ensure output directory exists
        os.makedirs(output_path, exist_ok=True)
        
        # Temporary file name (original format)
        temp_filename = f"{filename}.webm"
        temp_filepath = os.path.join(output_path, temp_filename)
        
        # Download audio in original format
        ys.download(output_path=output_path, filename=temp_filename)
        
        # Convert to MP3
        final_filename = f"{filename}.mp3"
        final_filepath = os.path.join(output_path, final_filename)
        audio = AudioSegment.from_file(temp_filepath)
        audio.export(final_filepath, format="mp3")
        
        # Remove the temporary file
        os.remove(temp_filepath)
        print(f"Downloaded and converted to MP3: {final_filename}")
    
    except Exception as e:
        print("----- Download failed: -----")
        print(f"URL: {url}")
        print(f"Error: {e}")

url_list = speeches["url"].to_list()


for i, url in enumerate(url_list, start=1):
    output_path = "./audio_files"  
    filename = f"speech_{i}_{speeches["date"][i-1]}_{speeches["state"][i-1]}"
    save_audio(url, output_path=output_path, filename=filename)

#initiate model to script the audio files
device = "cuda:0" if torch.cuda.is_available() else "cpu"
torch_dtype = torch.float16 if torch.cuda.is_available() else torch.float32

model_id = "openai/whisper-large-v3"

model = AutoModelForSpeechSeq2Seq.from_pretrained(
    model_id, torch_dtype=torch_dtype, low_cpu_mem_usage=False, use_safetensors=True
)
model.to(device)

processor = AutoProcessor.from_pretrained(model_id)

pipe = pipeline(
    "automatic-speech-recognition",
    model=model,
    tokenizer=processor.tokenizer,
    feature_extractor=processor.feature_extractor,
    torch_dtype=torch_dtype,
    device=device,
)

print("Model and pipe has been initiated.")

for i in range(len(url_list)):
    if speeches["trimming_needed"][i] == "Yes":
        continue
    else:
        print(f"Generating transcript of speech_{i+1}_{speeches["date"][i]}_{speeches["state"][i]}.mp3")

        result_txt = pipe(f"./audio_files/speech_{i+1}_{speeches["date"][i]}_{speeches["state"][i]}.mp3", return_timestamps=True)
        text_to_add = list(result_txt.values())[0]

        print("Transcript has been generated.")
        os.makedirs("text_files", exist_ok=True)
        file_path = os.path.join("text_files", f"speech_{i+1}_{speeches["date"][i]}_{speeches["state"][i]}.txt")
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(text_to_add)

        print("Transcript has been added to the folder.")
print("hello world")