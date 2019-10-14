import setuptools


with open("README.markdown", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name='Falderal',
    version='0.13',
    description='Definition of, and tools for using, the Falderal literate testing format',
    long_description=long_description,
    long_description_content_type="text/markdown",
    author='Chris Pressey',
    author_email='packages@catseye.tc',
    url='https://catseye.tc/node/Falderal',
    packages=['falderal'],
    package_dir={'': 'src'},
    scripts=['bin/falderal'],
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: BSD License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3",
        "Topic :: Software Development :: Testing",
    ],
)
