source tag.sh
replicas=$(($(kubectl get nodes | wc -l) - 1))
cat ../configs/analysis.yaml | envsubst | kubectl apply
#set the correct autoscaling
cat ../configs/broker.yaml | envsubst | kubectl apply
