ghci:
	stack ghci

ghcid:
	ghcid --height=8 --topmost "--command=stack ghci"

ghcid-test:
	ghcid --height=8 --topmost "--command=stack ghci --test"

