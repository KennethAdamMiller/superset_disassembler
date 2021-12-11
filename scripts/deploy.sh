source scripts/tag.sh
export replicas=$(($(kubectl get nodes | wc -l) - 1))
cat ../configs/analysis.yaml | envsubst | kubectl apply
cat ../configs/broker.yaml | envsubst | kubectl apply
