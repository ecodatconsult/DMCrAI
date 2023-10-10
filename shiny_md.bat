C:
cd "C:/git/\cameratraps"
call activate cameratraps-detector
set PYTHONPATH=%PYTHONPATH%;C:/git/\CameraTraps;C:/git/\ai4eutils;C:/git/\yolov5
python detection\run_detector_batch.py "C:/megadetector/md_v5a.0.0.pt" "C:\md_pics\beispieldaten" "C:\md_pics\beispieldaten\md_out.json" --recursive
