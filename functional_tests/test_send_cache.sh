source scripts/tag.sh
export replicas=1
cat ../configs/analysis.yaml | envsubst | kubectl apply
export test_size=1
cat ../configs/broker.yaml | envsubst | kubectl apply
