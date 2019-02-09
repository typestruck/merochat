requests
GET: params
	^ TeaRequest fromZnRequest: (ZnRequest get: '') pathParams: params