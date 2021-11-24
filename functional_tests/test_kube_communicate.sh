source scripts/tag.sh
export replicas=1
export test_size=1
cat ./configs/broker-deployment.yaml | envsubst | kubectl apply -f -
cat ./configs/broker-service.yaml | envsubst | kubectl apply -f -
cat ./configs/analysis-deployment.yaml | envsubst | kubectl apply -f -
