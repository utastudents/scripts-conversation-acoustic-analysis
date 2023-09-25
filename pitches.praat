writeInfoLine: " "

#@files: "c:\temp\wav","c:\temp\Pitch\"

@files: "D:\CallFriend\eng-n","d:\Pitch\CallFriend\eng-n"
@files: "D:\CallFriend\eng-s","d:\Pitch\CallFriend\eng-s"
@files: "D:\CallFriend\fra-q","d:\Pitch\CallFriend\fra-q"
@files: "D:\CallFriend\deu","d:\Pitch\CallFriend\deu"
@files: "D:\CallFriend\jpn","d:\Pitch\CallFriend\jpn"
@files: "D:\CallFriend\spa","d:\Pitch\CallFriend\spa"
@files: "D:\CallFriend\spa-c","d:\Pitch\CallFriend\spa-c"
@files: "D:\CallFriend\zho-t","d:\Pitch\CallFriend\zho-t"
@files: "D:\CallFriend\zho-m","d:\Pitch\CallFriend\zho-m"
@files: "D:\CallHome\ara","d:\Pitch\CallHome\ara"
@files: "D:\CallHome\zho","d:\Pitch\CallHome\zho"
@files: "D:\CallHome\eng","d:\Pitch\CallHome\eng"
@files: "D:\CallHome\deu","d:\Pitch\CallHome\deu"
@files: "D:\CallHome\jpn","d:\Pitch\CallHome\jpn"
@files: "D:\CallHome\spa","d:\Pitch\CallHome\spa"
@files: "D:\GECO\2track\mono","d:\Pitch\GECO\mono"
@files: "D:\GECO\2track\multi","d:\Pitch\GECO\multi"
@files: "D:\CGN\20151207_CGN_2_0_3\CGN_2.0.3\data\audio\wav\comp-c\nl","d:\Pitch\CGN\nl-cs"
@files: "D:\CGN\20151207_CGN_2_0_3\CGN_2.0.3\data\audio\wav\comp-c\vl","d:\Pitch\CGN\vl-cs"
@files: "D:\CGN\20151207_CGN_2_0_3\CGN_2.0.3\data\audio\wav\comp-d\nl","d:\Pitch\CGN\nl-cm"
@files: "D:\CGN\20151207_CGN_2_0_3\CGN_2.0.3\data\audio\wav\comp-d\vl","d:\Pitch\CGN\vl-cm"

procedure files: .indir$, .outdir$
    appendInfoLine: .indir$
    strings = Create Strings as file list: "list", .indir$  + "\*.wav"
    numberofFiles = Get number of strings
    for ifile to numberofFiles

        selectObject: strings 
        file$ = Get string: ifile
        Read from file: .indir$ + "\" + file$
        #strip off file ext and replace . with _ so we can reference the object later
        obj$ = replace$(left$(file$,length(file$)-4),".","_",0)
        appendInfoLine: obj$
        selectObject: "Sound " + obj$

        numch = Get number of channels
        if numch = 2
            Extract all channels

            @getPitch: 1, .outdir$, obj$
            @getPitch: 2, .outdir$, obj$

        endif
        selectObject: "Sound " + obj$
        Remove
    endfor

    selectObject: "Strings list"
    Remove
endproc

procedure getPitch: .ch, .outdir$, .obj$
    selectObject: "Sound " + obj$ + "_ch" + string$(.ch)
    To Pitch: 0, 75, 600
    selectObject: "Pitch " + .obj$ + "_ch" + string$(.ch)
    Save as text file: .outdir$ + "\" + .obj$ + "_ch" + string$(.ch) + ".Pitch"
    selectObject: "Pitch " + .obj$ + "_ch" + string$(.ch)
    Remove
    selectObject: "Sound " + .obj$ + "_ch" + string$(.ch)
    Remove
endproc

