SELECT p.parties, min(p.min_trxn_dt) AS min_trxn_dt
FROM (
	SELECT (d.ORIG_ID || '-' || d.BENEF_ID) AS parties, min(d.TRXN_DT_S) AS min_trxn_dt
	FROM d
	GROUP BY (d.ORIG_ID || '-' || d.BENEF_ID)
	UNION ALL
	SELECT (d.BENEF_ID || '-' || d.ORIG_ID) AS parties, min(d.TRXN_DT_S) AS min_trxn_dt
	FROM d
	GROUP BY (d.BENEF_ID || '-' || d.ORIG_ID)
) p
GROUP BY p.parties