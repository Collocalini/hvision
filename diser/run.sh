#cat test1 |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi .avi
#cat test1 |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi pipe:|mplayer -profile prof3 -
#./diser |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi pipe:|mplayer -profile prof3 -
#./diser --gnuplot-file plot.gpi --range-of-files 1..1000 --data-bypass-mode --data-file /home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -s 640x480 pipe:|mplayer -profile prof3 -
#./diser --gnuplot-file plot.gpi --range-of-files 1..1000 --data-process identity --data-file /home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -s 640x480 pipe:|mplayer -profile prof3 -
: << 'xxx'
./diser --gnuplot-file plot.gpi \
        --range-of-files 1..1000 \
        --data-process identity_i,derivative_f \
        --data-file /media/sda2/sda2/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data \
        |gnuplot -persist \
        |ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -y -s 1280x511 pipe: \
        |mplayer -idx -
xxx


./diser --gnuplot-file plot.gpi \
        --range-of-files 1..100 \
        --data-process identity_i,derivative_f  \
        --data-file /home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data \
        |gnuplot -persist \
        |ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -s 1280x511 pipe: \
        |mplayer -profile prof3 -

