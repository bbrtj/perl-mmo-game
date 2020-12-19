use v5.30;
use warnings;

use Test::More;
use Mojo::Collection;
use Game::Common::Container qw(add_to_container);
use Game::Ability::Parser;

package GameRepoMock {
	use Moo;

	sub load_ability_data
	{
		return Mojo::Collection->new(
			{
				id => 'test',
				attribute => 'ABA_FIRE',
				passive => 0,
				cost => 0,
				cooldown => 5,
				range => 50,
				target_self => 0,
				target_ally => 0,
				target_foe => 0,
				target_ground => 1,
				effect_type => 'AET_HEAL',
				effect_attribute => 'ABA_VVV',
				effect_group => 1,
				value => 50,
				deviation => 3,
			},
			{
				id => 'test',
				attribute => 'ABA_FIRE',
				passive => 0,
				cost => 0,
				cooldown => 5,
				range => 50,
				target_self => 0,
				target_ally => 0,
				target_foe => 0,
				target_ground => 1,
				effect_type => 'AET_COEF_INT',
				effect_attribute => 'ABA_VVV',
				effect_group => 1,
				value => 0.2,
				deviation => undef,
			},
			{
				id => 'test',
				attribute => 'ABA_FIRE',
				passive => 0,
				cost => 0,
				cooldown => 5,
				range => 50,
				target_self => 0,
				target_ally => 0,
				target_foe => 0,
				target_ground => 1,
				effect_type => 'AET_DURAT',
				effect_attribute => 'ABA_VVV',
				effect_group => 1,
				value => 3,
				deviation => undef,
			}
		);
	}
};

add_to_container(game_data_repo => GameRepoMock->new);
my $parsed = Game::Ability::Parser->parse;

ok exists $parsed->{test};
is scalar keys $parsed->%*, 1;

my $subject = $parsed->{test};

isa_ok $subject, "Game::Ability::Compiled";
isa_ok $subject->attribute, "Game::Ability::Attribute::Fire";
ok !$subject->passive;
is $subject->cost, 0;
is $subject->cooldown, 5;
is $subject->range, 50;
ok !$subject->target_self;
ok !$subject->target_ally;
ok !$subject->target_foe;
ok $subject->target_ground;

ok exists $subject->groups->{1};
is scalar keys $subject->groups->%*, 1;

$subject = $subject->group(1);
isa_ok $subject, "Game::Ability::Compiled::Group";
ok exists $subject->effects->[0];
is scalar $subject->effects->@*, 3;

for my $index (0 .. 2) {
	my $subject = $subject->effects->[$index];
	isa_ok $subject, "Game::Ability::Compiled::Effect";
	if ($subject->effect_type->isa("Game::Ability::EffectType::Healing")) {
		isa_ok $subject->attribute, "Game::Ability::Attribute::Inherit";
		is $subject->value, 50;
		is $subject->deviation, 3;
	}
	elsif ($subject->effect_type->isa("Game::Ability::EffectType::IntellectCoefficient")) {
		is $subject->value, "0.2";
		ok !defined $subject->deviation;
	}
	elsif ($subject->effect_type->isa("Game::Ability::EffectType::Duration")) {
		is $subject->value, 3;
		ok !defined $subject->deviation;
	}
	else {
		fail "Unknown effect type";
	}
}

done_testing;
