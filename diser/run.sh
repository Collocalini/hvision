#cat test1 |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi .avi
#cat test1 |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi pipe:|mplayer -profile prof3 -
#./diser |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi pipe:|mplayer -profile prof3 -
#./diser --gnuplot-file plot.gpi --range-of-files 1..1000 --data-bypass-mode --data-file /home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -s 640x480 pipe:|mplayer -profile prof3 -
#./diser --gnuplot-file plot.gpi --range-of-files 1..1000 --data-process identity --data-file /home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_Surveillance.avi/data_files/data |gnuplot -persist|ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -s 640x480 pipe:|mplayer -profile prof3 -


diser --gnuplot-file plot.gpi \
        --data-process max_derivative_in_range_xy_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > max_derivative_in_range_xy_f.plot

cat max_derivative_in_range_xy_f.plot |gnuplot -persist > max_derivative_in_range_xy_f.jpg


diser --gnuplot-file plot.gpi \
        --data-process min_derivative_in_range_xy_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > min_derivative_in_range_xy_f.plot

cat min_derivative_in_range_xy_f.plot |gnuplot -persist > min_derivative_in_range_xy_f.jpg

: << 'xxx'
diser --gnuplot-file plot.gpi \
        --data-process distance_between_extremums_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > distance_between_extremums_f.plot

cat distance_between_extremums_f.plot |gnuplot -persist > distance_between_extremums_f.jpg




diser --gnuplot-file plot.gpi \
        --data-process derivative_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > derivative_f.plot

cat derivative_f.plot |gnuplot -persist > derivative_f.jpg






diser --gnuplot-file plot.gpi \
        --data-process identity_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > identity_f.plot

cat identity_f.plot |gnuplot -persist > identity_f.jpg


diser --gnuplot-file plot.gpi \
        --data-process extremums_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > extremums_f.plot

cat extremums_f.plot |gnuplot -persist > extremums_f.jpg




diser --gnuplot-file plot.gpi \
        --data-process processor_x_2_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > processor_x_2_f.plot

cat processor_x_2_f.plot |gnuplot -persist > processor_x_2_f.jpg


diser --gnuplot-file plot.gpi \
        --data-process processor_x_2_2_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > processor_x_2_2_f.plot

cat processor_x_2_2_f.plot |gnuplot -persist > processor_x_2_2_f.jpg


diser --gnuplot-file plot.gpi \
        --data-process processor_x_2_3_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > processor_x_2_3_f.plot

cat processor_x_2_3_f.plot |gnuplot -persist > processor_x_2_3_f.jpg


diser --gnuplot-file plot.gpi \
        --data-process processor_xm_2_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > processor_xm_2_f.plot

cat processor_xm_2_f.plot |gnuplot -persist > processor_xm_2_f.jpg



diser --gnuplot-file plot.gpi \
        --data-process processor_xm_2_2_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > processor_xm_2_2_f.plot

cat processor_xm_2_2_f.plot |gnuplot -persist > processor_xm_2_2_f.jpg

diser --gnuplot-file plot.gpi \
        --data-process processor_xm_2_3_f\
        --use-columns 1:2 \
        --data-file xxx \
        --multipage-data-file 3 \
        --matrix-stacking \
        > processor_xm_2_3_f.plot

cat processor_xm_2_3_f.plot |gnuplot -persist > processor_xm_2_3_f.jpg



cat xxx |diser --gnuplot-file plot.gpi \
        --data-process extremums_f,processor_x_2_f,processor_x_2_2_f,processor_x_2_3_f,processor_xm_2_f,processor_xm_2_2_f,processor_xm_2_3_f\
        --use-columns 1:2 \
        --data-from-stdin 3 \
        #--repeat-frames-of-output 7 \
        #|gnuplot -persist \
        #|ffmpeg -f mjpeg -i pipe: -f avi -vcodec copy -y -s 1280x511 avi4


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
