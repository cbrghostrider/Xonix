all:
	ghc ./GamePlay/Xonix.hs -i./RenderingAPI:./GamePlay:./Keyboard:./Sounds
	echo "Run ./GamePlay/Xonix and enjoy!"
