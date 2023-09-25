
form Test 
  sentence indir c:\temp\wav
  sentence outdir c:\temp\TextGrid
  integer trunc 0
  real sound_min 0.03
  real silence_min 0.1
  real ints_ratio 0.01
endform

appendInfoLine: outdir$

@files: indir$, outdir$, trunc, sound_min, silence_min, ints_ratio

procedure files: .indir$, .outdir$, .trunc, .sound_min, .silence_min, .ints_ratio
    strings = Create Strings as file list: "list", .indir$  + "\*.wav"
    numberofFiles = Get number of strings
    for ifile to numberofFiles

        selectObject: strings 
        file$ = Get string: ifile
        Read from file: .indir$ + "\" + file$
        #strip off file ext and replace . with _ so we can reference the object later
        obj$ = replace$(left$(file$,length(file$)-4),".","_",0)
        #appendInfoLine: obj$
        
        selectObject: "Sound " + obj$

        numch = Get number of channels
        if numch = 2
            #start the NL calls after the beep and truncate all the CGN files before the end-of-call recording starts 
            if .trunc != 0
                if .trunc = 2
                    Extract part: 1.7, 566.5, "rectangular", 1, "yes"
                else 
                    #.trunc = 1
                    Extract part: 0, 566.5, "rectangular", 1, "yes"
                endif
                selectObject: "Sound " + obj$
                Remove
                selectObject: "Sound " + obj$ + "_part"
                Rename: obj$
            endif
            Extract all channels

            @silences: 1, .outdir$, obj$
            @silences: 2, .outdir$, obj$
            
            #combine the two textgrids (for reference)
            selectObject: "TextGrid " + obj$ + "_ch1", "TextGrid " + obj$ + "_ch2" 
            Merge
            Save as text file: .outdir$ + "\" + obj$ + "_" + string$(.sound_min) + "_" + string$(.silence_min) + "_" + string$(.ints_ratio) + "_both_ch.TextGrid"
            Remove
            selectObject: "TextGrid " + obj$ + "_ch1" 
            Remove
            selectObject: "TextGrid " + obj$ + "_ch2" 
            Remove
        endif
        selectObject: "Sound " + obj$
        Remove
    endfor

    selectObject: "Strings list"
    Remove
endproc

procedure silences: .ch, .outdir$, .obj$
    selectObject: "Sound " + .obj$ + "_ch" + string$(.ch)
    To Intensity: 200, 0.001, "yes"

    max_db = Get maximum: 0, 0, "Parabolic"
    max_amp = 10**(max_db/20)
    #find difference from amplitude 32768 - setting the sound/silence threshold to a percentage of the max amplitude
    db = -20*log10(max_amp/(ints_ratio * 32768))

    intsFile$ = .outdir$ + "\ints.txt"

    appendFileLine: intsFile$, .obj$ + " " + string$(.ch) + " " + string$(max_db) + " " + string$(max_amp) + " " + string$(db)

    To TextGrid (silences): db, silence_min, sound_min, "silent", "sounding"
    Save as text file: .outdir$ + "\" + .obj$ + "_ch" + string$(.ch) + ".TextGrid"
    selectObject: "Intensity " + .obj$ + "_ch" + string$(.ch)
    Remove
    selectObject: "TextGrid " + .obj$ + "_ch" + string$(.ch)
    #Remove
    selectObject: "Sound " + .obj$ + "_ch" + string$(.ch)
    Remove
endproc

