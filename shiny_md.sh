cd "/home/alex/git/MegaDetector"
source /home/alex/miniforge3/etc/profile.d/conda.sh
conda activate cameratraps-detector
export PYTHONPATH=/home/alex/git/MegaDetector:/home/alex/git/yolov5
python detection/run_detector_batch.py "/home/alex/git/md_model/md_v5a.0.0.pt" "/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data" "/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data/md_out.json" --recursive --resume_from_checkpoint "/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data/md_out.json"
echo 'This file should quickly disappear'>/home/alex/Documents/Projekte/FotofallenApp/DMCrAI/inst/test_data/megadetector_just_finished.txt
