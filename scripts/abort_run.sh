if [[ -z ${TAG} ]]; then
	echo "TAG variable unset"
	exit
fi	
kubectl delete job/analysis-${TAG}
kubectl delete job/broker-${TAG}
