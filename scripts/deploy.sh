source scripts/tag.sh
source scripts/feature_suffix.sh
cat ./configs/analysis-deployment.yaml | envsubst | kubectl apply -f -
cat ./configs/broker-service.yaml | envsubst | kubectl apply -f -
cat ./configs/broker-job.yaml | envsubst | kubectl apply -f -
