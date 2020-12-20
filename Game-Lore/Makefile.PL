use 5.030001;
use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => 'Game::Lore',
	VERSION_FROM => 'lib/Game/Lore.pm',
	PREREQ_PM => {
		'Game::Common' => 0,
		'Game::Schema' => 0,
		'Game::Repository' => 0,
		'Game::Unit' => 0,
	},
	AUTHOR => 'brtastic',
);
