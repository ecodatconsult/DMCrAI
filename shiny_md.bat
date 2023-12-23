cd "/home/alex/git/MegaDetector"
source shiny_md.bat/NA/NA/miniforge3/etc/profile.d/conda.sh
conda activate cameratraps-detector
export PYTHONPATH=/home/alex/git/MegaDetector:/home/alex/git/yolov5
python detection/run_detector_batch.py "/home/alex/git/md_model/md_v5a.0.0.pt" "/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data" "/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data/md_out.json" --recursive
echo 'This file should quickly disappear'>/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data/megadetector_just_finished.txt
