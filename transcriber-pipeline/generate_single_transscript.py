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


def save_audio(url, output_path, filename):
    try:
        yt = YouTube(url, 'WEB')
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

output_path = "/Users/barnabasepres/University/TDK/trump_speech_analysis/trump-speech-analysis/single_audio_file"  
filename = 'speech_32_"2024-09-29"_PA'
url = "https://www.youtube.com/watch?v=zUbDvg2gWXw&ab_channel=LiveNOWfromFOX"
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

print("Generating transcript of speech")
      
result_txt = pipe(f'./single_audio_file/speech_32_"2024-09-29"_PA.mp3', return_timestamps=True)
text_to_add = list(result_txt.values())[0]

print("Transcript has been generated.")
os.makedirs("single_text_file", exist_ok=True)
file_path = os.path.join("single_text_file", f'speech_32_"2024-09-29"_PA.txt')
with open(file_path, 'w', encoding='utf-8') as f:
    f.write(text_to_add)

print("Transcript has been added to the folder.")
