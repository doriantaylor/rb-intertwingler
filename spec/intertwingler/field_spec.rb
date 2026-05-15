require 'spec_helper'

require 'intertwingler/field'

describe Intertwingler::Field do
  # base class to_s should complain about not being implemented
  it 'should complain if you attempt to serialize it' do
    expect { Intertwingler::Field.new('test').to_s }.to raise_error(NotImplementedError)
  end

  it 'should properly handle an integer header' do
    val = Intertwingler::Field['CONTENT_LENGTH'].new '31337'

    expect(val).to be_a(Intertwingler::Field::NonNegativeInteger)
    expect(val.value).to eql 31337
    expect(val.to_s).to eql '31337'
  end

  it 'should parse an individual type with parameters' do
    val = Intertwingler::Field['Content-Type'].new 'Text/HTML;version=5;q=7;foo="lol wut"'
    # should downcase and rearrange and truncate the integer
    expect(val.to_s).to eq 'text/html;foo="lol wut";version=5'
  end

  it 'should parse a naïve list' do
    val = Intertwingler::Field['Content-Encoding'].new 'gzip, base64'

    expect(val).to be_a(Intertwingler::Field::List)
    expect(val.value).to eq %i[gzip base64]
  end

  it 'should correctly normalize a set' do
    val = Intertwingler::Field['Accept-Ranges'].new 'dongs, bytes'

    expect(val).to be_a(Intertwingler::Field::Set)
    # expect(val.value).to eq Set[:bytes, :dongs]
    expect(val.to_s).to eq 'bytes, dongs'
  end

  it 'should correctly handle pairs of values' do
    val = Intertwingler::Field['Cache-Control'].new 'must-revalidate, max-age=500'
    expect(val).to be_a(Intertwingler::Field::Pairs)

    # pairs should be sorted by key
    expect(val.to_s).to eq 'max-age=500, must-revalidate'
  end

  it 'should handle ranked/weighted values' do
    # weighted values should be normalized and sorted descending by weight
    val = Intertwingler::Field['Accept-Language'].new 'fr, en-US;q=0.8, en-CA;q=7'
    expect(val.to_s).to eq 'en-ca, fr, en-us;q=0.8'
  end

  it 'should handle an `Accept` header' do
    val = Intertwingler::Field['HTTP_ACCEPT'].new 'application/x-POTATO;q=0, Text/HTML;q=1;version=6, text/html;q=0.9, application/xhtml+xml, application/xml;q=3.14159, */*;q=0.001'
    # media type parameters should be sorted lexically ex `q` which

    expect(val.to_s).to eq 'application/xhtml+xml, application/xml, text/html;version=6, text/html;q=0.9, */*;q=0.001, application/x-potato;q=0'

  end

  it 'should handle a `Date` header' do
    # TBD lol
  end

  it 'should handle a `Link` header' do
    # TBD lol
  end

  it 'should handle an `Authorization` header' do
    # TBD lol
  end
end
