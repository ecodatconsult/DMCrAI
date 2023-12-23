cd "/home/alex/git/MegaDetector"
source /home/alex/miniforge3/etc/profile.d/conda.sh
conda activate cameratraps-detector
export PYTHONPATH=/home/alex/git/MegaDetector:/home/alex/git/yolov5
python detection/run_detector_batch.py "/home/alex/git/md_model/md_v5a.0.0.pt" "/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data/wvb_ff_5034_220809" "/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data/wvb_ff_5034_220809/md_out.json" --recursive
echo 'This file should quickly disappear'>/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data/wvb_ff_5034_220809/megadetector_just_finished.txt
