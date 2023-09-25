import os
import pandas as pd
import pitch

def main():

    codes_csv = 'c:\\users\\david\\documents\\synced\\dissertation\\corpus\\codes.csv'
    codes = pd.read_csv(codes_csv)
    print(codes)

    tempfile = 'C:\\Temp\\temp_pitch.csv'
    outfile = 'C:\\Temp\\pitch_all.csv'
    with open(outfile, 'w') as out:
        out_str = 'subcorpus_cd,conv,ch,freq,pt\n'  
        out.write(out_str) 
    
    for index, row in codes.iterrows():
        print(row.PitchDir)
        PitchDir = os.fsencode(row.PitchDir)
        if os.path.exists(PitchDir):   
            for file in os.listdir(PitchDir):
                #convert from bytes to string
                s_file = file.decode('windows-1252')
                if s_file.find('Pitch') != -1:
                    #print(file)
                    i = s_file.find('_')
                    match row.Corpus:
                        case 'GECO':
                            #get 3-character filename after first underscore                
                            conv_nm = s_file[i+1:i+4]
                            ch = s_file[i+14:i+15]
                        case 'CGN':
                            #8-character filename before first underscore
                            conv_nm = s_file[i-8:i]
                            ch = s_file[i+3:i+4]    
                        case _:
                            #4-character filename before first underscore
                            conv_nm = s_file[i-4:i]
                            ch = s_file[i+3:i+4]    
                    #print(conv_nm + " " + ch)   
                    
                    this_pitch = pitch.praatPitch(PitchDir + b'\\' + file,subcorpus=row.Code,conv=conv_nm,ch=ch )
                    this_pitch.to_csv(tempfile)
                    with open(tempfile, 'r') as temp:
                        with open(outfile, 'a') as out:
                            out.write(temp.read()) 
                            out.write('\n')

if __name__ == '__main__':
    main()
    