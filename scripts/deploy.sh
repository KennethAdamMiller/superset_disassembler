source tag.sh
cat ../configs/analysis.yaml | envsubst | kubectl apply
#set the correct number of initial replicas
#set the correct autoscaling
cat ../configs/broker.yaml | envsubst | kubectl apply
