package My::Sub::HandlesVia::Toolkit::Mouse::PackageTrait;

use v5.36;

use Mouse::Role;
use Sub::HandlesVia::Toolkit::Mouse;

sub _shv_toolkit {
        'Sub::HandlesVia::Toolkit::Mouse',
}

around add_attribute => sub {
        my ($next, $self, @args) = (shift, shift, @_);
        my ($spec, $attrobj, $attrname);
        if (@args == 1) {
                $spec = $attrobj = $_[0];
                $attrname = $attrobj->name;
        }
        elsif (@args == 2) {
                ($attrname, $spec) = @args;
        }
        else {
                my %spec;
                ($attrname, %spec) = @args;
                $spec = \%spec;
        }
        $spec->{provides}{shv} = $self->_shv_toolkit->clean_spec($self->name, $attrname, $spec)
                unless $spec->{provides}{shv};
        my $attr = $self->$next($attrobj ? $attrobj : ($attrname, %$spec));
        if ($spec->{provides}{shv} and $self->isa('Mouse::Meta::Class')) {
                $self->_shv_toolkit->install_delegations(+{
                        %{ $spec->{provides}{shv} },
                        target => $self->name,
                });
        }
        return $attr;
};

1;

