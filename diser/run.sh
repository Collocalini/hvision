#cat test1 |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi .avi
#cat test1 |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi pipe:|mplayer -profile prof3 -
#./diser |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi pipe:|mplayer -profile prof3 -
#./diser --gnuplot-file plot.gpi --range-of-files 1..1000 --data-bypass-mode --data-file /home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -s 640x480 pipe:|mplayer -profile prof3 -
#./diser --gnuplot-file plot.gpi --range-of-files 1..1000 --data-process identity --data-file /home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -s 640x480 pipe:|mplayer -profile prof3 -

cat xxx |diser --gnuplot-file plot.gpi \
        --data-process extremums_f,processor_x_2_f \
        --use-columns 1:2 \
        --data-from-stdin 3 \
        --repeat-frames-of-output 2 \
       # |gnuplot -persist \
       # |ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -y -s 1280x511 avi2

: << 'xxx'
./diser --gnuplot-file plot.gpi \
        --data-process identity_f,derivative_f \
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        |gnuplot -persist \
        |ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -y -s 1280x511 .avi #pipe: \
        #|mplayer -idx -
xxx

#cat xxx | runhaskell -i../src/ ../src/Main.hs --gnuplot-file plot.gpi \

: << 'xxx'
cat xxx |./diser --gnuplot-file plot.gpi \
        --data-process extremums_f,distance_between_extremums_f \
        --use-columns 1:2 \
        --data-from-stdin 3 \
        --repeat-frames-of-output 2 \
        |gnuplot -persist \
        |ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -y -s 1280x511 avi1 #pipe: \
        #|mplayer -idx -
#--data-process identity_f,distance_between_extremums_f \
#--data-process identity_f,extremums_f,distance_between_extremums_f \
xxx

: << 'xxx'
./diser --gnuplot-file plot.gpi \
        --data-process identity_f,derivative_f  \
        --data-file xxx \
        --use-columns 1:2 \
        --multipage-data-file 3 \
        #|gnuplot -persist \
        #|ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -s 1280x511 pipe: \
        #mplayer -profile prof3 -
xxx

#--range-of-files 1..1000 \
#
#--data-file /home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data \
#--data-file /media/sda2/sda2/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data \
#
