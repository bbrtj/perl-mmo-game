use 5.030001;
use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => 'Game::Common',
	VERSION_FROM => 'lib/Game/Common.pm',
	PREREQ_PM => {
		'Moo' => 0,
		'Type::Tiny' => 0,
		'Mojolicious' => 0,
		'Mojo::Pg' => 0,
		'Minion' => 0,
	},
	AUTHOR => 'brtastic',
);
