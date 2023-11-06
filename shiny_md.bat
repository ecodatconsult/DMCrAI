C:
cd "C:/git/\cameratraps"
call activate cameratraps-detector
set PYTHONPATH=%PYTHONPATH%;C:/git/\CameraTraps;C:/git/\ai4eutils;C:/git/\yolov5
python detection\run_detector_batch.py "C:/megadetector/md_v5a.0.0.pt" "C:\md_pics\md_pics_neu" "C:\md_pics\md_pics_neu\md_out.json" --recursive --resume_from_checkpoint "C:\md_pics\md_pics_neu\md_out.json"
echo This file should quickly disappear>C:\md_pics\md_pics_neu\megadetector_just_finished.txt"
