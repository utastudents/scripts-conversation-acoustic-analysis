# scripts-conversation-acoustic-analysis
Scripts to analyze audio files of conversations

Code

    scrape_CF.py                download the wav files from the CallFriend corpus
    scrape_CH.py                download the wav files from the CallHome corpus
    code to create same_pitch.csv
        c_pitch.sql             create table in a relational database (I used SQL Server)
        c_pitch.xml             bcp format file
        pitches.praat           extract pitch for each channel of each file
        pitch.py                create a class for pitch data
        pitch_all.py            combine the pitch data into pitch_all.csv, with a row for each channel for each conversation for each subcorpus
        bcp c_pitch.txt         load pitch_all.csv into table
        same_pitch_ratio.sql    count how often the two channels are near each other in pitch, even if there is a systematic offset to the match
    single_speaker_segments.py  Main module of this project. Note that it requires praat-textgrids (https://pypi.org/project/praat-textgrids/)
        Main steps:
            call_praat          create sound/silence textgrids for each file in the corpora, for each of the parameters specified
            textgrids_to_csv    pull the sound times from the Praat textgrids into sound_times_csv
            find_turn_begin     analyze the sound times for transitions and create files for the turn times on each channel and for transitions 
            trans_textgrids     create textgrids of transitions to help explain the data
    silences-param.praat        create sound/silence textgrid for each channel for a file
    dfs-CF,CH only.R            create data frames from the various data files
    graph functions.R           graph functions to evaluate models
    tests.R                     statistical tests
    graphs, tables.R            graphs and tables

Manually created files

    codes.csv 
        information about subcorpora
        logical PK is Code (col H)
        columns
            Corpus          Corpus name
            Language        Language name
            Description     Subcorpus full description
            LangCd          Language code (ISO 639-3)
            RegionLangCd    LangCd, with an abbreviation for the region, if one is specified
            Mode            Monomodal (phone) or Multimodal (face-to-face)
            Designation     Distinguishing feature (Required only for some corpora)
            Code            Subcorpus code, made up of abbreviation of Corpus + RegionLangCd
            wavDir          Directory where the audio files are located
            TextGridDir     Directory where the text grids will be placed
            PitchDir        Directory where the pitch files will be placed
        
    params.xlsx 
        acoustic parameters to be evaluated
        logical PK is folder (col H) - iteration column is not actually used
        script only uses sheet 1, so you can keep a library of parameters in sheet 2 and just paste some of them into sheet 1 to check results
        columns
            iter        iteration (used just to help distinguish which row is which)
            sound       sound threshold in s
            silence     silence threshold in s
            ints        intency threshold as a ratio
            sound_ms    sound threshold in ms
            sil_ms      silence threshold in ms
            ints_pct    intensity threshold as a percentage
            folder      the concatenation of the above three fields to be appended onto the end of the value of TextGridDir in codes.csv
