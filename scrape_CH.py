import urllib.request
from bs4 import BeautifulSoup
import shutil
import os
import ctypes
import time

def get_free_space_mb(dirname):
    """Return folder/drive free space (in megabytes)."""
    #assume windows
    free_bytes = ctypes.c_ulonglong(0)
    ctypes.windll.kernel32.GetDiskFreeSpaceExW(ctypes.c_wchar_p(dirname), None, None, ctypes.pointer(free_bytes))
    return free_bytes.value / 1024 / 1024

def download_file (url, file_link, lang):
    print ('download_file ' + url + file_link)
    if get_free_space_mb ("D:\TEMP") > 50000: 
        ind = file_link.find('.')
        file_num = file_link[0:ind]
        #print (file_num)
        wav_dir='D:\\CallHome\\' + lang + '\\'
        wav_file=wav_dir + file_num + '.wav'
        if not os.path.exists(wav_dir):
            os.makedirs(wav_dir)
        print(wav_file)  
        if os.path.isfile(wav_file):
            print ("already exists")
        else:
            #add sleep so the site won't restrict us
            time.sleep(22)
            with urllib.request.urlopen(url + file_link) as response, open(wav_file, 'wb') as out_file:   
                shutil.copyfileobj(response, out_file)


def drill (url):

    page = urllib.request.urlopen(url)
    soup = BeautifulSoup(page, 'html.parser')
    print (soup)
    for link in soup.find_all('a'):
        file_link = link.get('href')
        #print (url + file_link)
        if file_link.find('?') == -1 and file_link[0] != '/':
            link_idx = (url + file_link).find('CallHome') 
            if link_idx != -1:
                end_idx = (url + file_link)[link_idx + 9:].find('/')
                if end_idx != -1:
                    lang =  (url + file_link)[link_idx + 9:link_idx + 9 + end_idx]
                    #print(lang)
                    if file_link.find('.') == -1:
                        drill (url + file_link)
                    else:
                        if (url + file_link).find(lang) != -1 and file_link[-4:] == '.wav':
                            download_file (url, file_link, lang)

pg1 = 'https://media.talkbank.org/ca/CallHome/'
drill(pg1)
