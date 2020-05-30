# TODO: use stow for symlinks

fix_ssh_permissions() {
	echo "Setting ssh file permissions to 600"
	cd ~/.ssh
	chmod 600 id_rsa
	chmod 600 id_rsa.pub
	echo
}

install_dotfiles() {
	echo "Setting up dotfiles"
	# clone
	# cd ~ && git clone git@github.com:chloelee767/dotfiles.git
	dotfiles_dir=dotfiles/

	softlink() {
		# arguments
		dest=$1
		name=$2
		is_dir=$3

		echo $name

		cd ~/$dest
		if [[ $is_dir == true ]]; then
			rm -fr $name
		else
			rm -f $name
		fi

		ln -s ~/$dotfiles_dir$dest$name $name
	}	

	# soft links
	# softlink "" .profile false
	softlink "" .zshrc false
	# softlink "" .doom.d true
	# softlink .config/ starship.toml false
	# softlink .config/ nvim true

	echo
}

install_yay () {
    mkdir ~/programs
    cd ~/programs
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
}

# install_emacs27() {
# 	# TODO
# }

install_fonts() {
    yay -S ttf-iosevka ttf-hack
}

install_doom() {
    sudo pacman -S fd ripgrep cmake # doom dependencies
	git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d  
	~/.emacs.d/bin/doom install
}

install_neovim() {
	sudo pacman -S neovim

	# setup
	sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

	echo "open .config/nvim/init.vim and run :PlugInstall"
}

install_terminal() {
	sudo pacman -S kitty
}

# fix_ssh_permissions
install_dotfiles
# install_doom
# install_neovim
# install_yay
# install_fonts
