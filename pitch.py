#code modified from praat-textgrids
import codecs
import io
import struct
from collections import OrderedDict, namedtuple

#def main():
    
    
Point = namedtuple('Point', ['frequency', 'xpos'])

class Tier(list):
    '''Tier is a list of either Interval or Point objects.'''

    def __init__(self, data=None, xmin=0.0, xmax=0.0, point_tier=False):
        if not data:
            data = []
        # Use data for xmin and xmax unless they are explicitly given
        if data and xmin == 0.0 and xmax == 0.0:
            xmin = data[0].xmin
            xmax = data[-1].xmax
        self.xmin = float(xmin)
        self.xmax = float(xmax)
        if self.xmin < 0 or self.xmax < 0:
            raise ValueError('value not float or is < 0.0')
        self.is_point_tier = point_tier
        super().__init__(data)

    def __add__(self, tier):
        '''Concatenate tiers.'''
        # Only a tier or the empty list can be concatenated with a tier
        if tier and not isinstance(tier, Tier):
            raise TypeError('incompatible types')
        # Concatenate only tiers of the same type
        if self.is_point_tier != tier.is_point_tier:
            raise TypeError('tier types differ')
        # Do not add a tier at the end which begins before this one ends.
        if self.xmax > tier.xmin:
            raise ValueError('Cannot extend a tier with one that begins before this tier ends: {max} > {min}', 
                self.xmax, tier.xmin)
        return Tier(super().__add__(tier))

    def offset_time(self, offset):
        """Move all timestamps in this Tier, including xmin and xmax, by offset."""
        self.xmin += offset
        self.xmax += offset

        if self.is_point_tier:
            for point in self:
                point.xpos += offset
        else:
            for interval in self:
                interval.offset_time(offset)

    @property
    def tier_type(self):
        '''Return tier type as string (for convenience).'''
        return 'PointTier' if self.is_point_tier else 'IntervalTier'

class praatPitch(OrderedDict):
    '''praatPitch is a dict of framess (keys) and candidates (values).'''

    def __init__(self, filename=None, xmin=0.0, subcorpus=None, conv=None, ch=None):
        self.xmin = self.xmax = xmin
        self.filename = filename
        self.subcorpus = subcorpus
        self.conv = conv
        self.ch = ch
        if self.filename:
            self.read(self.filename)              

    def read(self, filename):
        with open(filename, 'r') as infile:
            data = infile.read()
        self.parse(data)

    def parse(self, data):
        text = ['File type = "ooTextFile"', 'Object class = "Pitch 1"', '']
        buff = [s.strip() for s in data.split('\n')]
        # Check and then discard header
        if buff[:len(text)] != text:
            raise TypeError
        buff = buff[len(text):]
        self._parse_text(buff) 
        
    def _parse_text(self, data):
        '''Parse text pitch files. Not intended to be used directly.'''
        def grab(s):
            return s.split(' = ')[1]
        self.xmin, self.xmax = [float(grab(s)) for s in data[:2]]
        self.nx = int  (grab(data[2]))
        self.dx = float(grab(data[3]))
        
        tier = Tier(point_tier=True)
        p = 9
        for i in range(self.nx):
            #print(data[p])
            #print(data[p+1])
            intensity = float(grab(data[p]))
            nCandidates = int(grab(data[p+1]))
            p += 4
            for j in range(nCandidates):
                if j == 0:
                    frequency = float(grab(data[p]))
                    #print(str(frequency))
                    #strength = float(grab(data[p + 1]))
                    #tier.append(Point(frequency, (self.dx * i) + self.xmin))
                    tier.append(Point(frequency, i))
                p += 3            
        self['Pitch'] = tier

    def to_csv(self, filename):
        '''Format tier data as CSV, each row a separate string.'''
        #skip row if freq is 0 in order to limit size of file
        #output = ['"{}","{}",{},{},{}'.format(self.subcorpus, self.conv, self.ch, p.frequency, p.xpos) for p in self['Pitch'] if p.frequency > 0]
        output = ['{},{},{},{},{}'.format(self.subcorpus, self.conv, self.ch, p.frequency, p.xpos) for p in self['Pitch'] if p.frequency > 0]
        with open(filename, 'w') as csvfile:
            csvfile.write('\n'.join(output))


         





