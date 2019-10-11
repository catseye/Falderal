from distutils.core import setup


setup(
    name='Falderal',
    version='0.13',
    description='Definition of, and tools for using, the Falderal literate testing format',
    author='Chris Pressey',
    author_email='packages@catseye.tc',
    url='https://catseye.tc/node/Falderal',
    packages=['falderal'],
    package_dir={'': 'src'},
    scripts=['bin/falderal'],
)
